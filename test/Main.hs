{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Hspec
import Text.RawString.QQ
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)
import WSHS hiding (main)
import Control.Monad (forM_)
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import System.IO.Temp (withSystemTempDirectory)
import Data.Set qualified as Set

-- | Run a WS action with a minimal configuration
-- TODO this really should take the cfg opts settings and initial state
-- somehow, or have some other way to have it parameterized. The fact that whatever
-- WS prop that is using this function doesn't need to have those things set is not
-- a stable property. Figure that out at some point.
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap "" "" }
  let cfg = Configuration
        { configDir = ""
        , configRepoUrl = ""
        , configRepoOrigin = ""
        , configRepoBranch = ""
        , properties = []
        }
  let settings = Settings { opts = opts, configuration = cfg }
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)
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
        result <- checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)
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
        result <- expandPath "~"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` (not . T.isInfixOf "~")

      it "expands tilde in path" $ do
        result <- expandPath "~/foo/bar"
        result `shouldSatisfy` T.isPrefixOf "/"
        result `shouldSatisfy` T.isSuffixOf "/foo/bar"

      it "leaves absolute paths unchanged" $ do
        result <- expandPath "/usr/local/bin"
        result `shouldBe` "/usr/local/bin"

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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
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
        result <- checkSingleDotfile cfg (T.pack srcFile) (T.pack destFile)
        result `shouldBe` False
