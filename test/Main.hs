{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main, main2) where

import Test.Hspec
import Text.RawString.QQ
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Text.Lazy.Encoding qualified as TL


-- import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)
import WSHS hiding (main)
import Control.Monad (forM_)
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import System.IO.Temp (withSystemTempDirectory)
import Data.Set qualified as Set
import Shh.Internal (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure, (<<<), toArgs, asArg, displayCommand, Cmd)
import Data.ByteString.Lazy hiding (writeFile, readFile, length)
import Data.Either (isRight)
import qualified SudoSpec
import qualified GitHomeDirSpec

-- | Run a WS action with a minimal configuration
-- TODO this really should take the cfg opts settings and initial state
-- somehow, or have some other way to have it parameterized. The fact that whatever
-- WS prop that is using this function doesn't need to have those things set is not
-- a stable property. Figure that out at some point.
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap "" "", sudoCache = False, sudoPassFile = Nothing, verbose = False }
  let settings = Settings { opts = opts, sudoCmd = "sudo" }
  let initialState = WSState { props = Set.empty }
  fst <$> runStateT (runReaderT (unWS action) settings) initialState

-- | Run a WS action with a specific sudoCmd in Settings
runWSWith :: String -> WS a -> IO a
runWSWith sc action = do
  let opts = Options { command = Bootstrap "" "", sudoCache = False, sudoPassFile = Nothing, verbose = False }
  let settings = Settings { opts = opts, sudoCmd = sc }
  let initialState = WSState { props = Set.empty }
  fst <$> runStateT (runReaderT (unWS action) settings) initialState

-- | Create a test file with content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = writeFile path content

-- | Create a test directory with a file inside
createTestDir :: FilePath -> IO ()
createTestDir path = do
  createDirectoryIfMissing True path
  writeFile (path </> "testfile.txt") "test content"

configText :: Text
configText = [r|
configDir: "~/.wshs"
configRepoUrl: "https://github.com/joelmccracken/dotfiles"
configRepoOrigin: "git@github.com:joelmccracken/dotfiles.git"
configRepoBranch: "main"
properties:
- type: GitHomeDir
  params:
    gitDir: ".git-homedir"
|]



main2 :: IO ()
main2 = hspec $ do
  it "experimentation space" $ do
    liftIO $ tryFailure $ toArgs (asArg "env") "touch" "foobar"
    liftIO $ tryFailure $ exe ["env", "bash", "-c", "echo something great > foobar2"]
    capt <- liftIO $ ( exe ["env", "cat", "foobar2"] |> captureTrim)

    capt `shouldBe` "something great"

    let pth = "foobar2"
    foo <- liftIO $ do
      let x = ["echo", "the file is '", capt, "' at filen named ", pth ]
      useSudo <- pure False
      putStrLn "before"
      if useSudo
        then pure ((exe $ "sudo" : x) :: [ByteString])
        else pure (exe $ "env" : x)
    putStrLn $ show $ displayCommand $ exe foo
    putStrLn "before1"
    _ <- exe foo
    putStrLn "after1"

    -- userCmd <- mkPrivCmd "sudo" WriteAccess "root_only" ["bash", "-c", "echo $USER"]
    -- user <- liftIO $ userCmd |> captureTrim
    -- -- TL.putStrLn $ TL.decodeLatin1 res
    -- user `shouldBe` "root"

main :: IO ()
main = hspec $ do
  describe "DotfileConfig" $ do
    let
      withTempSrcAndDest fn =
        withSystemTempDirectory "wshs-test" $ \tmpDir -> do
          let srcDir = tmpDir </> "src"
          let destDir = tmpDir </> "dest"
          createDirectoryIfMissing True srcDir
          createDirectoryIfMissing True destDir
          fn srcDir destDir

    describe "checkSingleDotfile" $ do
      it "returns True for correct symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` True

      it "returns False for symlink pointing to wrong target" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let wrongFile = srcDir </> "wrongfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile wrongFile "wrong content"
        createSymbolicLink wrongFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

      it "returns False for missing symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

      it "returns True for correct copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` True

      it "returns False for copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "different content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

      it "returns False for symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

      it "returns True for correct directory copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcSubDir = srcDir </> "subdir"
        let destSubDir = destDir </> "subdir"
        createTestDir srcSubDir
        createTestDir destSubDir

        let cfg = DotfileConfig
              { src = "subdir"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = True
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)
        result `shouldBe` True

      it "returns False for directory copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcSubDir = srcDir </> "subdir"
        let destSubDir = destDir </> "subdir"
        createTestDir srcSubDir
        createDirectoryIfMissing True destSubDir
        writeFile (destSubDir </> "testfile.txt") "different content"

        let cfg = DotfileConfig
              { src = "subdir"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = True
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)
        result `shouldBe` False

    describe "DotfilesP checker" $ do
      it "returns True when all dotfiles are correct" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile1 = srcPath </> "file1"
        let srcFile2 = srcPath </> "file2"
        let destFile1 = destPath </> "file1"
        let destFile2 = destPath </> "file2"
        createTestFile srcFile1 "content1"
        createTestFile srcFile2 "content2"
        createSymbolicLink srcFile1 destFile1
        createTestFile destFile2 "content2"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ DotfileConfig { src = "file1", dest = Nothing, dot = False, sort = Symlink, dir = False }
                  , DotfileConfig { src = "file2", dest = Nothing, dot = False, sort = Copy, dir = False }
                  ]
              }
        result <- runWS $ checker dotfilesP
        result `shouldBe` True

      it "returns False when a dotfile is incorrect" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile1 = srcPath </> "file1"
        let destFile1 = destPath </> "file1"
        createTestFile srcFile1 "content1"
        createTestFile destFile1 "wrong content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ DotfileConfig { src = "file1", dest = Nothing, dot = False, sort = Copy, dir = False }
                  ]
              }
        result <- runWS $ checker dotfilesP
        result `shouldBe` False

    describe "dest field handling" $ do
      it "uses absolute dest path directly without prepending destDir" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile = srcPath </> "file1"
        let destFile = destPath </> "absolute-dest"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just "/some/other/path/"  -- Should be ignored for absolute dest
              , files =
                  [ DotfileConfig
                      { src = "file1"
                      , dest = Just (T.pack destFile)  -- Absolute path
                      , dot = False
                      , sort = Copy
                      , dir = False
                      }
                  ]
              }
        result <- runWS $ checker dotfilesP
        result `shouldBe` True

      it "prepends destDir to relative dest path" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile = srcPath </> "file1"
        let destFile = destPath </> "custom-name"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ DotfileConfig
                      { src = "file1"
                      , dest = Just "custom-name"  -- Relative path
                      , dot = False
                      , sort = Copy
                      , dir = False
                      }
                  ]
              }
        result <- runWS $ checker dotfilesP
        result `shouldBe` True

      it "ignores dot prefix when dest is explicit" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile = srcPath </> "file1"
        let destFile = destPath </> "custom-name"  -- No dot prefix
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ DotfileConfig
                      { src = "file1"
                      , dest = Just "custom-name"
                      , dot = True  -- Should be ignored when dest is set
                      , sort = Copy
                      , dir = False
                      }
                  ]
              }
        result <- runWS $ checker dotfilesP
        result `shouldBe` True

    describe "expandPath" $ do
      it "expands tilde to home directory" $ do
        result <- runWS $ expandPath "~"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` (not . T.isInfixOf "~")

      it "expands tilde in path" $ do
        result <- runWS $ expandPath "~/foo/bar"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` T.isSuffixOf "/foo/bar"

      it "leaves absolute paths unchanged" $ do
        result <- runWS $ expandPath "/usr/local/bin"
        result `shouldBe` "/usr/local/bin"

      it "expands $HOME to home directory" $ do
        result <- runWS $ expandPath "$HOME"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` (not . T.isInfixOf "$")

      it "expands $HOME in path" $ do
        result <- runWS $ expandPath "$HOME/foo/bar"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` T.isSuffixOf "/foo/bar"

    describe "broken symlink handling" $ do
      it "detects broken symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile
        removeFile srcFile

        isLink <- pathIsSymbolicLink destFile
        exists <- doesPathExist destFile
        isLink `shouldBe` True
        exists `shouldBe` False

    describe "mode switching" $ do
      it "detects symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

      it "detects regular file when Symlink mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False

    describe "computeDotfileDiff" $ do
      it "returns DotfileCorrect for correct symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileCorrect

      it "returns DotfileCorrect for correct copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileCorrect

      it "returns DotfileMissing when destination doesn't exist" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileMissing

      it "returns DotfileBrokenSymlink for broken symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let oldTarget = srcDir </> "oldtarget"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile oldTarget "old content"
        createSymbolicLink oldTarget destFile
        removeFile oldTarget  -- Break the symlink

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileBrokenSymlink

      it "returns DotfileWrong for symlink pointing to wrong target" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let wrongFile = srcDir </> "wrongfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile wrongFile "wrong content"
        createSymbolicLink wrongFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileWrong

      it "returns DotfileWrong for copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "different content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileWrong

      it "returns DotfileWrong for symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileWrong

      it "returns DotfileSrcMissing when source doesn't exist" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "nonexistent"
        let destFile = destDir </> "testfile"

        let cfg = DotfileConfig
              { src = "nonexistent"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        result <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` DotfileSrcMissing (T.pack srcFile)

    describe "computeDotfilePaths" $ do
      it "computes correct paths with default destDir" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Nothing  -- defaults to "~/"
              , files = []
              }
        let cfg = DotfileConfig
              { src = "vimrc"
              , dest = Nothing
              , dot = True
              , sort = Symlink
              , dir = False
              }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/vimrc"
        -- dest will have ~ expanded, so just check it ends correctly
        dest `shouldSatisfy` T.isSuffixOf ".vimrc"

      it "computes correct paths with explicit destDir" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Just "/home/user/"
              , files = []
              }
        let cfg = DotfileConfig
              { src = "bashrc"
              , dest = Nothing
              , dot = True
              , sort = Symlink
              , dir = False
              }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/bashrc"
        dest `shouldBe` "/home/user/.bashrc"

      it "computes correct paths with explicit dest" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Just "/home/user/"
              , files = []
              }
        let cfg = DotfileConfig
              { src = "vim"
              , dest = Just "custom-vim"
              , dot = False
              , sort = Symlink
              , dir = False
              }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/vim"
        dest `shouldBe` "/home/user/custom-vim"

      it "uses absolute dest path directly" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Just "/home/user/"
              , files = []
              }
        let cfg = DotfileConfig
              { src = "gitconfig"
              , dest = Just "/etc/gitconfig"
              , dot = False
              , sort = Copy
              , dir = False
              }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/gitconfig"
        dest `shouldBe` "/etc/gitconfig"

    describe "applyDotfileFix" $ do
      it "creates symlink for DotfileMissing" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        runWS $ applyDotfileFix cfg (T.pack srcFile) (T.pack destFile) DotfileMissing

        exists <- doesPathExist destFile
        exists `shouldBe` True
        isLink <- pathIsSymbolicLink destFile
        isLink `shouldBe` True

      it "creates copy for DotfileMissing with Copy mode" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Copy
              , dir = False
              }
        runWS $ applyDotfileFix cfg (T.pack srcFile) (T.pack destFile) DotfileMissing

        exists <- doesPathExist destFile
        exists `shouldBe` True
        isLink <- pathIsSymbolicLink destFile
        isLink `shouldBe` False
        content <- readFile destFile
        content `shouldBe` "content"

      it "removes broken symlink and creates new one for DotfileBrokenSymlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let oldTarget = srcDir </> "oldtarget"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile oldTarget "old"
        createSymbolicLink oldTarget destFile
        removeFile oldTarget  -- Break the symlink

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        runWS $ applyDotfileFix cfg (T.pack srcFile) (T.pack destFile) DotfileBrokenSymlink

        exists <- doesPathExist destFile
        exists `shouldBe` True
        isLink <- pathIsSymbolicLink destFile
        isLink `shouldBe` True
        -- Verify it points to the right target now
        diffResult <- runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)
        diffResult `shouldBe` DotfileCorrect

      it "backs up wrong file and creates new one for DotfileWrong" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "correct content"
        createTestFile destFile "wrong content"

        let cfg = DotfileConfig
              { src = "testfile"
              , dest = Nothing
              , dot = False
              , sort = Symlink
              , dir = False
              }
        runWS $ applyDotfileFix cfg (T.pack srcFile) (T.pack destFile) DotfileWrong

        -- Original dest should now be correct
        exists <- doesPathExist destFile
        exists `shouldBe` True
        isLink <- pathIsSymbolicLink destFile
        isLink `shouldBe` True

        -- A backup file should exist
        backupFiles <- listDirectory destDir
        length backupFiles `shouldSatisfy` (> 1)  -- testfile + backup

  describe "fileContentsCheck" $ do
    let withTempDir fn =
          withSystemTempDirectory "wshs-test" $ \tmpDir -> fn tmpDir

    it "returns True when file exists with matching contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "test content\nline 2\n"
      writeFile testFile content
      result <- runWS $ fileContentsCheck (T.pack testFile) (T.pack content)
      result `shouldBe` True

    it "returns False when file exists with different contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      result <- runWS $ fileContentsCheck (T.pack testFile) "different content"
      result `shouldBe` False

    it "returns False when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "nonexistent"
      result <- runWS $ fileContentsCheck (T.pack testFile) "some content"
      result `shouldBe` False

    it "returns True for empty file with empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      result <- runWS $ fileContentsCheck (T.pack testFile) ""
      result `shouldBe` True

    it "returns False for empty file with non-empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      result <- runWS $ fileContentsCheck (T.pack testFile) "some content"
      result `shouldBe` False

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let content = "line 1\nline 2\nline 3\n"
      writeFile testFile content
      result <- runWS $ fileContentsCheck (T.pack testFile) (T.pack content)
      result `shouldBe` True

  describe "fileContentsFix" $ do
    let withTempDir fn =
          withSystemTempDirectory "wshs-test" $ \tmpDir -> fn tmpDir

    it "returns Nothing when file already has correct contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "correct content"
      writeFile testFile content
      result <- runWS $ fileContentsFix (T.pack testFile) (T.pack content)
      result `shouldBe` Nothing
      -- Verify no backup was created
      files <- listDirectory tmpDir
      length files `shouldBe` 1

    it "returns Just backupPath when file exists with wrong contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      result `shouldSatisfy` \case
        Just path -> not (T.null path)
        Nothing -> False
      -- Verify file was updated
      newContent <- readFile testFile
      newContent `shouldBe` "new content"
      -- Verify backup exists
      files <- listDirectory tmpDir
      length files `shouldBe` 2  -- original + backup

    it "returns Just empty string when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "newfile"
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      result `shouldBe` Just ""
      -- Verify file was created
      exists <- doesPathExist testFile
      exists `shouldBe` True
      content <- readFile testFile
      content `shouldBe` "new content"

    it "backup file contains original contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let originalContent = "original content"
      writeFile testFile originalContent
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      case result of
        Just backupPath -> do
          backupContent <- readFile (T.unpack backupPath)
          backupContent `shouldBe` originalContent
        Nothing -> expectationFailure "Expected Just backupPath"

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let originalContent = "line 1\nline 2\n"
      let newContent = "new line 1\nnew line 2\nnew line 3\n"
      writeFile testFile originalContent
      result <- runWS $ fileContentsFix (T.pack testFile) (T.pack newContent)
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False
      -- Verify new content
      updatedContent <- readFile testFile
      updatedContent `shouldBe` newContent

  describe "mkPrivCmd" $ do
    let withTempDir fn = withSystemTempDirectory "wshs-test" $ \tmpDir -> fn tmpDir

    it "uses env prefix when path is user-owned (no sudo needed)" $ withTempDir $ \tmpDir -> do
      -- Path is user-owned → needsSudo returns False → exe ("env" : args)
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "sudo" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo hello > " <> outFile]
      _ <- exe (T.unpack <$> args)
      content <- readFile outFile
      content `shouldBe` "hello\n"

    it "uses injected sudo command when needs check returns True (injected as env)" $ withTempDir $ \tmpDir -> do
      -- inject "env" as the sudo command so the command succeeds even if sudo branch is taken
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "env" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo injected > " <> outFile]
      _ <- exe (T.unpack <$> args)
      content <- readFile outFile
      content `shouldBe` "injected\n"

    it "returned Cmd can be chained with |> to capture output" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "captured content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result <- exe (T.unpack <$> args) |> captureTrim
      result `shouldBe` "captured content"

    it "returned Cmd can be chained with &> devNull" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "some content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result <- tryFailure $ exe (T.unpack <$> args) &> devNull
      result `shouldSatisfy` isRight

  describe "privCmd" $ do
    let withTempDir fn = withSystemTempDirectory "wshs-test" $ \tmpDir -> fn tmpDir

    it "runs WriteAccess command on user-owned path without sudo" $ withTempDir $ \tmpDir -> do
      let outFile = tmpDir </> "out.txt"
      result <- runWS $ privCmd WriteAccess (T.pack tmpDir)
                          ["bash", "-c", T.pack $ "echo write-ok > " <> outFile]
      result `shouldSatisfy` isRight
      content <- readFile outFile
      content `shouldBe` "write-ok\n"

    it "reads sudoCmd from Settings (injected as env) for write path" $ withTempDir $ \tmpDir -> do
      -- sudoCmd = "env" means even if sudo were needed, env is used — command succeeds
      let outFile = tmpDir </> "out.txt"
      result <- runWSWith "env" $ privCmd WriteAccess (T.pack tmpDir)
                                    ["bash", "-c", T.pack $ "echo env-sudo > " <> outFile]
      result `shouldSatisfy` isRight
      content <- readFile outFile
      content `shouldBe` "env-sudo\n"

    it "runs ReadAccess command on user-owned file" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "read-ok"
      result <- runWS $ privCmd ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result `shouldSatisfy` isRight

    it "fails when the command itself fails" $ withTempDir $ \tmpDir -> do
      result <- runWS $ privCmd WriteAccess (T.pack tmpDir) ["false"]
      result `shouldSatisfy` \case
        Left _  -> True
        Right _ -> False

  describe "sudo functionality" $  do
    SudoSpec.spec

  describe "GitHomeDirCloneP" $
    GitHomeDirSpec.spec
