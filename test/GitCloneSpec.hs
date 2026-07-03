{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GitCloneSpec (spec) where

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
import Shh.Internal (exe)

import WSHS.Types
import WSHS.Properties.GitClone (GitCloneP (..))
import Util

-- | Run a WS action with a minimal configuration
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = "", workstation = "" }
  let settings = Settings { opts = opts, sudoCmd = "sudo" }
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) settings)) initialState

-- | Run a git command, ignoring its result.
git :: [String] -> IO ()
git args = exe ("git" : args)

-- | Populate a bare remote repo with:
--   - branch "main" containing "bashrc" and nested "config/foo/bar.conf"
--   - branch "feature" additionally containing "feature.txt"
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

    -- A second branch with an extra file, used to test branch selection.
    git ["-C", stagingDir, "checkout", "-b", "feature"]
    writeFile (stagingDir </> "feature.txt") "# feature\n"
    git ["-C", stagingDir, "add", "."]
    git ["-C", stagingDir, "commit", "-m", "feature"]
    git ["-C", stagingDir, "push", "origin", "feature"]

-- | Created ONCE for the whole spec (via aroundAll). Builds the bare remote and
-- populates it. The remote is only ever read from by checker/fixer, so it is
-- safe to share across all examples.
withRemote :: (FilePath -> IO ()) -> IO ()
withRemote action =
  withSystemTempDirectory "wshs-remote" $ \remoteRoot -> do
    let remoteDir = remoteRoot </> "remote"
    createDirectoryIfMissing True remoteDir
    -- Point the bare repo's HEAD at "main" so a no-branch clone checks it out.
    git ["init", "--bare", "-b", "main", remoteDir]
    setupRemote remoteDir
    action remoteDir

-- | Created PER TEST (via aroundWith). Provides a fresh clone destination path
-- (intentionally not created, so the fixer's @git clone@ creates it), paired
-- with the shared remoteDir.
withPerTestDirs :: ActionWith (FilePath, FilePath, GitCloneP) -> ActionWith FilePath
withPerTestDirs inner remoteDir =
  withSystemTempDirectory "wshs-test" $ \rootDir -> do
    let clonePath = rootDir </> "clone"  -- intentionally not created yet
    let p = mkProp remoteDir clonePath Nothing
    inner (remoteDir, clonePath, p)

-- | Build a property pointing at the given dirs.
mkProp :: FilePath -> FilePath -> Maybe Text -> GitCloneP
mkProp remoteDir clonePath branch = GitCloneP
  { repoUrl = T.pack remoteDir
  , path    = T.pack clonePath
  , branch  = branch
  }

spec :: Spec
spec =
  aroundAll withRemote $
  aroundWith withPerTestDirs $
  describe "GitCloneP" $ do

  -- ── Checker ──────────────────────────────────────────────────────────────

  describe "checker" $ do

    it "returns False when the path is missing" $ \(_remoteDir, _clonePath, p) -> do
      -- clonePath was never created, so checker should return False
      shouldBeM False $ runWS $ checker p

    it "returns False when the path exists but is not a git repo" $ \(_remoteDir, clonePath, p) -> do
      createDirectoryIfMissing True clonePath
      shouldBeM False $ runWS $ checker p

    it "returns False when remote URL does not match" $ \(_remoteDir, _clonePath, p) -> do
      runWS $ fixer p
      let wrongP = p { repoUrl = "file:///nonexistent" }
      shouldBeM False $ runWS $ checker wrongP

    it "returns True when cloned with a matching remote" $ \(_remoteDir, _clonePath, p) -> do
      runWS $ fixer p
      shouldBeM True $ runWS $ checker p

  -- ── Fixer ─────────────────────────────────────────────────────────────────

  describe "fixer" $ do

    it "clones root-level files into the path" $ \(_remoteDir, clonePath, p) -> do
      runWS $ fixer p
      shouldBeM True $ doesPathExist (clonePath </> "bashrc")
      shouldBeM "# bashrc\n" $ readFile (clonePath </> "bashrc")

    it "clones nested files" $ \(_remoteDir, clonePath, p) -> do
      runWS $ fixer p
      shouldBeM True $ doesPathExist (clonePath </> "config" </> "foo" </> "bar.conf")
      shouldBeM "# bar\n" $ readFile (clonePath </> "config" </> "foo" </> "bar.conf")

    it "checks out the default branch when no branch is given" $ \(_remoteDir, clonePath, p) -> do
      -- The default branch ("main") does not contain feature.txt.
      runWS $ fixer p
      shouldBeM False $ doesPathExist (clonePath </> "feature.txt")

    it "checks out the requested branch when one is given" $ \(_remoteDir, clonePath, p) -> do
      let featureP = p { branch = Just "feature" }
      runWS $ fixer featureP
      shouldBeM True $ doesPathExist (clonePath </> "feature.txt")
      shouldBeM "# feature\n" $ readFile (clonePath </> "feature.txt")

    it "produces a repo that the checker accepts" $ \(_remoteDir, _clonePath, p) -> do
      runWS $ fixer p
      shouldBeM True $ runWS $ checker p
