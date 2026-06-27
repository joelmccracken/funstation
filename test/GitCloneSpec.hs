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
withPerTestDirs :: ActionWith (FilePath, FilePath) -> ActionWith FilePath
withPerTestDirs inner remoteDir =
  withSystemTempDirectory "wshs-test" $ \rootDir -> do
    let clonePath = rootDir </> "clone"  -- intentionally not created yet
    inner (remoteDir, clonePath)

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

    it "returns False when the path is missing" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      -- clonePath was never created, so checker should return False
      result <- runWS $ checker p
      result `shouldBe` False

    it "returns False when the path exists but is not a git repo" $ \(remoteDir, clonePath) -> do
      createDirectoryIfMissing True clonePath
      let p = mkProp remoteDir clonePath Nothing
      result <- runWS $ checker p
      result `shouldBe` False

    it "returns False when remote URL does not match" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      let wrongP = p { repoUrl = "file:///nonexistent" }
      result <- runWS $ checker wrongP
      result `shouldBe` False

    it "returns True when cloned with a matching remote" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      result <- runWS $ checker p
      result `shouldBe` True

  -- ── Fixer ─────────────────────────────────────────────────────────────────

  describe "fixer" $ do

    it "clones root-level files into the path" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      exists <- doesPathExist (clonePath </> "bashrc")
      exists `shouldBe` True
      content <- readFile (clonePath </> "bashrc")
      content `shouldBe` "# bashrc\n"

    it "clones nested files" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      exists <- doesPathExist (clonePath </> "config" </> "foo" </> "bar.conf")
      exists `shouldBe` True
      content <- readFile (clonePath </> "config" </> "foo" </> "bar.conf")
      content `shouldBe` "# bar\n"

    it "checks out the default branch when no branch is given" $ \(remoteDir, clonePath) -> do
      -- The default branch ("main") does not contain feature.txt.
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      featureExists <- doesPathExist (clonePath </> "feature.txt")
      featureExists `shouldBe` False

    it "checks out the requested branch when one is given" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath (Just "feature")
      runWS $ fixer p
      exists <- doesPathExist (clonePath </> "feature.txt")
      exists `shouldBe` True
      content <- readFile (clonePath </> "feature.txt")
      content `shouldBe` "# feature\n"

    it "produces a repo that the checker accepts" $ \(remoteDir, clonePath) -> do
      let p = mkProp remoteDir clonePath Nothing
      runWS $ fixer p
      result <- runWS $ checker p
      result `shouldBe` True
