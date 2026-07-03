{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GitHomeDirSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as Set
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Shh.Internal (exe, captureTrim, (|>))

import WSHS.Types
import WSHS.Properties.GitHomeDir (GitHomeDirP (..))
import Util

-- | Run a WS action with a minimal configuration
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = "", workstation = "" }
  let settings = Settings { opts = opts, sudoCmd = "sudo" }
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) settings)) initialState

-- | Run a git command in a specific directory, ignoring its result.
git :: [String] -> IO ()
git args = exe $ ("git" : args)

-- | Populate a bare remote repo with two committed files:
--   - "bashrc"            (root level)
--   - "config/foo/bar.conf" (nested)
setupRemote :: FilePath -> IO ()
setupRemote remoteDir =
  withSystemTempDirectory "wshs-staging" $ \stagingDir -> do
    git ["-c", "init.defaultBranch=main", "init", stagingDir]
    git ["-C", stagingDir, "config", "user.email", "test@example.com"]
    git ["-C", stagingDir, "config", "user.name",  "Test User"]
    git ["-C", stagingDir, "remote", "add", "origin", remoteDir]

    writeFile (stagingDir </> "bashrc") "# bashrc\n"
    createDirectoryIfMissing True (stagingDir </> "config" </> "foo")
    writeFile (stagingDir </> "config" </> "foo" </> "bar.conf") "# bar\n"

    git ["-C", stagingDir, "add", "."]
    git ["-C", stagingDir, "commit", "-m", "initial"]
    git ["-C", stagingDir, "push", "origin", "main"]

-- | Created ONCE for the whole spec (via aroundAll). Builds the bare remote
-- and populates it. The remote is only ever read from by checker/fixer, so it
-- is safe to share across all examples.
withRemote :: (FilePath -> IO ()) -> IO ()
withRemote action =
  withSystemTempDirectory "wshs-remote" $ \remoteRoot -> do
    let remoteDir = remoteRoot </> "remote"
    createDirectoryIfMissing True remoteDir
    git ["init", "--bare", remoteDir]
    setupRemote remoteDir
    action remoteDir

-- | Created PER TEST (via aroundWith). Provides a fresh gitDir (not pre-created,
-- so the fixer will properly initialise it) and a fresh fakeHome, paired with
-- the shared remoteDir.
withPerTestDirs :: ActionWith (FilePath, FilePath, FilePath, GitHomeDirP) -> ActionWith FilePath
withPerTestDirs inner remoteDir =
  withSystemTempDirectory "wshs-test" $ \rootDir -> do
    let gitDir   = rootDir </> "gitdir"   -- intentionally not created yet
        fakeHome = rootDir </> "home"
    createDirectoryIfMissing True fakeHome
    let p = mkProp remoteDir gitDir fakeHome Nothing
    inner (remoteDir, gitDir, fakeHome, p)

-- | Build a property pointing at the given dirs.
mkProp :: FilePath -> FilePath -> FilePath -> Maybe Text -> GitHomeDirP
mkProp remoteDir gitDir fakeHome afterChange = GitHomeDirP
  { gitDir         = T.pack gitDir
  , remoteUrl      = T.pack remoteDir
  , branch         = "main"
  , homeDir        = Just (T.pack fakeHome)
  , runAfterChange = afterChange
  }

spec :: Spec
spec =
  aroundAll withRemote $
  aroundWith withPerTestDirs $
  describe "GitHomeDirP" $ do

  -- ── Checker ──────────────────────────────────────────────────────────────

  describe "checker" $ do

    it "returns False when git dir is missing" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- gitDir path was never created, so checker should return False
      shouldBeM False $ runWS $ checker p

    it "returns False when remote URL is wrong" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- Run fixer to set up a valid repo, then check with wrong URL
      runWS $ fixer p
      let wrongP = p { remoteUrl = "file:///nonexistent" }
      shouldBeM False $ runWS $ checker wrongP

    it "returns False when tracked files are absent from homeDir" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- Only init + fetch, don't checkout
      git ["init", "--bare", gitDir]
      git ["--git-dir", gitDir, "remote", "add", "origin", remoteDir]
      git ["--git-dir", gitDir, "fetch", "origin"]
      shouldBeM False $ runWS $ checker p

    it "returns True when fully set up" $ \(remoteDir, gitDir, fakeHome, p) -> do
      runWS $ fixer p
      shouldBeM True $ runWS $ checker p

  -- ── Fixer ─────────────────────────────────────────────────────────────────

  describe "fixer" $ do

    it "copies root-level files into homeDir" $ \(remoteDir, gitDir, fakeHome, p) -> do
      runWS $ fixer p
      shouldBeM True $ doesPathExist (fakeHome </> "bashrc")
      shouldBeM "# bashrc\n" $ readFile (fakeHome </> "bashrc")

    it "copies nested files, creating missing subdirs" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- Neither config/ nor config/foo/ exist in fakeHome
      runWS $ fixer p
      shouldBeM True $ doesPathExist (fakeHome </> "config" </> "foo" </> "bar.conf")
      shouldBeM "# bar\n" $ readFile (fakeHome </> "config" </> "foo" </> "bar.conf")

    it "copies nested file when parent dir already exists" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- Pre-create config/foo/ with an unrelated file
      createDirectoryIfMissing True (fakeHome </> "config" </> "foo")
      writeFile (fakeHome </> "config" </> "foo" </> "unrelated") "unrelated\n"
      runWS $ fixer p
      shouldBeM True $ doesPathExist (fakeHome </> "config" </> "foo" </> "bar.conf")
      -- Unrelated file must be untouched
      shouldBeM "unrelated\n" $ readFile (fakeHome </> "config" </> "foo" </> "unrelated")

    it "leaves existing conflicting files alone" $ \(remoteDir, gitDir, fakeHome, p) -> do
      -- Put a local version of bashrc in fakeHome before fixer runs
      writeFile (fakeHome </> "bashrc") "local-version\n"
      runWS $ fixer p
      shouldBeM "local-version\n" $ readFile (fakeHome </> "bashrc")

    it "runs runAfterChange script when changes are made" $ \(remoteDir, gitDir, fakeHome, p) -> do
      withSystemTempDirectory "wshs-sentinel" $ \sentinelDir -> do
        let sentinelFile = sentinelDir </> "sentinel"
        -- Write a script that touches the sentinel file
        scriptFile <- do
          let sf = sentinelDir </> "after.sh"
          writeFile sf $ "touch " <> sentinelFile <> "\n"
          return sf
        let p' = p { runAfterChange = Just (T.pack scriptFile) }
        runWS $ fixer p'
        shouldBeM True $ doesPathExist sentinelFile

    it "does not run runAfterChange when nothing changed" $ \(remoteDir, gitDir, fakeHome, p) -> do
      withSystemTempDirectory "wshs-sentinel" $ \sentinelDir -> do
        let sentinelFile = sentinelDir </> "sentinel"
        let sf = sentinelDir </> "after.sh"
        writeFile sf $ "touch " <> sentinelFile <> "\n"
        let p' = p { runAfterChange = Just (T.pack sf) }
        -- First run: sets everything up, sentinel created
        runWS $ fixer p'
        -- Reset sentinel
        removeFile sentinelFile
        -- Second run: checker would pass, nothing to do
        runWS $ fixer p'
        shouldBeM False $ doesPathExist sentinelFile

    it "git status works from homeDir when gitDir is relative '.git'" $ \(remoteDir, _gitDir, fakeHome, p) -> do
      let p' = p { gitDir = ".git" }
      runWS $ fixer p'
      let expectedStatus = mconcat
            [ "On branch main\n"
            , "Your branch is up to date with 'origin/main'.\n"
            , "\n"
            , "nothing to commit, working tree clean"
            ]
      shouldBeM expectedStatus $ withCurrentDirectory fakeHome $
        exe (["git", "-c", "color.ui=never", "status"] :: [String]) |> captureTrim

    it "is idempotent: second fixer run leaves files unchanged" $ \(remoteDir, gitDir, fakeHome, p) -> do
      runWS $ fixer p
      content1 <- readFile (fakeHome </> "bashrc")
      runWS $ fixer p
      content2 <- readFile (fakeHome </> "bashrc")
      content1 `shouldBe` content2
      shouldBeM True $ runWS $ checker p
