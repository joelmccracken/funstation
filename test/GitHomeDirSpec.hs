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
withPerTestDirs :: ActionWith (FilePath, FilePath, FilePath) -> ActionWith FilePath
withPerTestDirs inner remoteDir =
  withSystemTempDirectory "wshs-test" $ \rootDir -> do
    let gitDir   = rootDir </> "gitdir"   -- intentionally not created yet
        fakeHome = rootDir </> "home"
    createDirectoryIfMissing True fakeHome
    inner (remoteDir, gitDir, fakeHome)

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

    it "returns False when git dir is missing" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      -- gitDir path was never created, so checker should return False
      result <- runWS $ checker p
      result `shouldBe` False

    it "returns False when remote URL is wrong" $ \(remoteDir, gitDir, fakeHome) -> do
      -- Run fixer to set up a valid repo, then check with wrong URL
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      let wrongP = p { remoteUrl = "file:///nonexistent" }
      result <- runWS $ checker wrongP
      result `shouldBe` False

    it "returns False when tracked files are absent from homeDir" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      -- Only init + fetch, don't checkout
      git ["init", "--bare", gitDir]
      git ["--git-dir", gitDir, "remote", "add", "origin", remoteDir]
      git ["--git-dir", gitDir, "fetch", "origin"]
      result <- runWS $ checker p
      result `shouldBe` False

    it "returns True when fully set up" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      result <- runWS $ checker p
      result `shouldBe` True

  -- ── Fixer ─────────────────────────────────────────────────────────────────

  describe "fixer" $ do

    it "copies root-level files into homeDir" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      exists <- doesPathExist (fakeHome </> "bashrc")
      exists `shouldBe` True
      content <- readFile (fakeHome </> "bashrc")
      content `shouldBe` "# bashrc\n"

    it "copies nested files, creating missing subdirs" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      -- Neither config/ nor config/foo/ exist in fakeHome
      runWS $ fixer p
      exists <- doesPathExist (fakeHome </> "config" </> "foo" </> "bar.conf")
      exists `shouldBe` True
      content <- readFile (fakeHome </> "config" </> "foo" </> "bar.conf")
      content `shouldBe` "# bar\n"

    it "copies nested file when parent dir already exists" $ \(remoteDir, gitDir, fakeHome) -> do
      -- Pre-create config/foo/ with an unrelated file
      createDirectoryIfMissing True (fakeHome </> "config" </> "foo")
      writeFile (fakeHome </> "config" </> "foo" </> "unrelated") "unrelated\n"
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      exists <- doesPathExist (fakeHome </> "config" </> "foo" </> "bar.conf")
      exists `shouldBe` True
      -- Unrelated file must be untouched
      unrelated <- readFile (fakeHome </> "config" </> "foo" </> "unrelated")
      unrelated `shouldBe` "unrelated\n"

    it "leaves existing conflicting files alone" $ \(remoteDir, gitDir, fakeHome) -> do
      -- Put a local version of bashrc in fakeHome before fixer runs
      writeFile (fakeHome </> "bashrc") "local-version\n"
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      content <- readFile (fakeHome </> "bashrc")
      content `shouldBe` "local-version\n"

    it "runs runAfterChange script when changes are made" $ \(remoteDir, gitDir, fakeHome) -> do
      withSystemTempDirectory "wshs-sentinel" $ \sentinelDir -> do
        let sentinelFile = sentinelDir </> "sentinel"
        -- Write a script that touches the sentinel file
        scriptFile <- do
          let sf = sentinelDir </> "after.sh"
          writeFile sf $ "touch " <> sentinelFile <> "\n"
          return sf
        let p = (mkProp remoteDir gitDir fakeHome (Just (T.pack scriptFile)))
        runWS $ fixer p
        exists <- doesPathExist sentinelFile
        exists `shouldBe` True

    it "does not run runAfterChange when nothing changed" $ \(remoteDir, gitDir, fakeHome) -> do
      withSystemTempDirectory "wshs-sentinel" $ \sentinelDir -> do
        let sentinelFile = sentinelDir </> "sentinel"
        let sf = sentinelDir </> "after.sh"
        writeFile sf $ "touch " <> sentinelFile <> "\n"
        let p = mkProp remoteDir gitDir fakeHome (Just (T.pack sf))
        -- First run: sets everything up, sentinel created
        runWS $ fixer p
        -- Reset sentinel
        removeFile sentinelFile
        -- Second run: checker would pass, nothing to do
        runWS $ fixer p
        exists <- doesPathExist sentinelFile
        exists `shouldBe` False

    it "git status works from homeDir when gitDir is relative '.git'" $ \(remoteDir, _gitDir, fakeHome) -> do
      let p = mkProp remoteDir ".git" fakeHome Nothing
      runWS $ fixer p
      output <- withCurrentDirectory fakeHome $
        exe (["git", "-c", "color.ui=never", "status"] :: [String]) |> captureTrim
      let expectedStatus = mconcat
            [ "On branch main\n"
            , "Your branch is up to date with 'origin/main'.\n"
            , "\n"
            , "nothing to commit, working tree clean"
            ]
      output `shouldBe` expectedStatus

    it "is idempotent: second fixer run leaves files unchanged" $ \(remoteDir, gitDir, fakeHome) -> do
      let p = mkProp remoteDir gitDir fakeHome Nothing
      runWS $ fixer p
      content1 <- readFile (fakeHome </> "bashrc")
      runWS $ fixer p
      content2 <- readFile (fakeHome </> "bashrc")
      content1 `shouldBe` content2
      result <- runWS $ checker p
      result `shouldBe` True
