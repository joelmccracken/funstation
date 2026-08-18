{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Properties.DotfilesSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

import Funstation hiding (main, failLeft)
import TestHelpers

-- | Create a test file with content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = writeFile path content

-- | Create a test directory with a file inside
createTestDir :: FilePath -> IO ()
createTestDir path = do
  createDirectoryIfMissing True path
  writeFile (path </> "testfile.txt") "test content"

-- | Base 'DotfileConfig' for tests; override individual fields with
-- record-update syntax, e.g. @testDotfileConfig { sort = Copy }@.
testDotfileConfig :: DotfileConfig
testDotfileConfig = DotfileConfig
  { src = "testfile"
  , dest = Nothing
  , dot = False
  , sort = Symlink
  , dir = False
  }

spec :: Spec
spec = do
  describe "DotfileConfig" $ do
    let
      withTempSrcAndDest fn =
        withTempDir "funstation-test" $ \tmpDir -> do
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

        shouldBeM True $ runWS $
          checkSingleDotfile testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns False for symlink pointing to wrong target" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let wrongFile = srcDir </> "wrongfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile wrongFile "wrong content"
        createSymbolicLink wrongFile destFile

        shouldBeM False $ runWS $
          checkSingleDotfile testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns False for missing symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        shouldBeM False $ runWS $
          checkSingleDotfile testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns True for correct copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        shouldBeM True $ runWS $
          checkSingleDotfile (testDotfileConfig { sort = Copy }) (T.pack srcFile) (T.pack destFile)

      it "returns False for copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "different content"

        shouldBeM False $ runWS $
          checkSingleDotfile (testDotfileConfig { sort = Copy }) (T.pack srcFile) (T.pack destFile)

      it "returns False for symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        shouldBeM False $ runWS $
          checkSingleDotfile (testDotfileConfig { sort = Copy }) (T.pack srcFile) (T.pack destFile)

      it "returns True for correct directory copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcSubDir = srcDir </> "subdir"
        let destSubDir = destDir </> "subdir"
        createTestDir srcSubDir
        createTestDir destSubDir

        let cfg = testDotfileConfig { src = "subdir", sort = Copy, dir = True }
        shouldBeM True $ runWS $ checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)

      it "returns False for directory copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcSubDir = srcDir </> "subdir"
        let destSubDir = destDir </> "subdir"
        createTestDir srcSubDir
        createDirectoryIfMissing True destSubDir
        writeFile (destSubDir </> "testfile.txt") "different content"

        let cfg = testDotfileConfig { src = "subdir", sort = Copy, dir = True }
        shouldBeM False $ runWS $ checkSingleDotfile cfg (T.pack srcSubDir) (T.pack destSubDir)

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
                  [ testDotfileConfig { src = "file1" }
                  , testDotfileConfig { src = "file2", sort = Copy }
                  ]
              }
        shouldBeM True $ runWS $ checker dotfilesP

      it "returns False when a dotfile is incorrect" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile1 = srcPath </> "file1"
        let destFile1 = destPath </> "file1"
        createTestFile srcFile1 "content1"
        createTestFile destFile1 "wrong content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ testDotfileConfig { src = "file1", sort = Copy }
                  ]
              }
        shouldBeM False $ runWS $ checker dotfilesP

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
                  [ testDotfileConfig
                      { src = "file1"
                      , dest = Just (T.pack destFile)  -- Absolute path
                      , sort = Copy
                      }
                  ]
              }
        shouldBeM True $ runWS $ checker dotfilesP

      it "prepends destDir to relative dest path" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile = srcPath </> "file1"
        let destFile = destPath </> "custom-name"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ testDotfileConfig
                      { src = "file1"
                      , dest = Just "custom-name"  -- Relative path
                      , sort = Copy
                      }
                  ]
              }
        shouldBeM True $ runWS $ checker dotfilesP

      it "ignores dot prefix when dest is explicit" $ withTempSrcAndDest $ \srcPath destPath -> do
        let srcFile = srcPath </> "file1"
        let destFile = destPath </> "custom-name"  -- No dot prefix
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let dotfilesP = DotfilesP
              { srcDir = T.pack srcPath
              , destDir = Just (T.pack destPath)
              , files =
                  [ testDotfileConfig
                      { src = "file1"
                      , dest = Just "custom-name"
                      , dot = True  -- Should be ignored when dest is set
                      , sort = Copy
                      }
                  ]
              }
        shouldBeM True $ runWS $ checker dotfilesP

    describe "mode switching" $ do
      it "detects symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        shouldBeM False $ runWS $
          checkSingleDotfile (testDotfileConfig {sort = Copy }) (T.pack srcFile) (T.pack destFile)

      it "detects regular file when Symlink mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        shouldBeM False $ runWS $
          checkSingleDotfile testDotfileConfig (T.pack srcFile) (T.pack destFile)

    describe "computeDotfileDiff" $ do
      it "returns DotfileCorrect for correct symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        shouldBeM DotfileCorrect $ runWS $
          computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns DotfileCorrect for correct copy" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "content"

        let cfg = testDotfileConfig { sort = Copy }
        shouldBeM DotfileCorrect $ runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)

      it "returns DotfileMissing when destination doesn't exist" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        shouldBeM DotfileMissing $ runWS $ computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns DotfileBrokenSymlink for broken symlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let oldTarget = srcDir </> "oldtarget"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile oldTarget "old content"
        createSymbolicLink oldTarget destFile
        removeFile oldTarget  -- Break the symlink

        shouldBeM DotfileBrokenSymlink $ runWS $
          computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns DotfileWrong for symlink pointing to wrong target" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let wrongFile = srcDir </> "wrongfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile wrongFile "wrong content"
        createSymbolicLink wrongFile destFile

        shouldBeM DotfileWrong $ runWS $
          computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns DotfileWrong for copy with different content" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile destFile "different content"

        shouldBeM DotfileWrong $ runWS $
          computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "returns DotfileWrong for symlink when Copy mode expected" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createSymbolicLink srcFile destFile

        let cfg = testDotfileConfig { sort = Copy }
        shouldBeM DotfileWrong $ runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)

      it "returns DotfileSrcMissing when source doesn't exist" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "nonexistent"
        let destFile = destDir </> "testfile"

        let cfg = testDotfileConfig { src = "nonexistent" }
        shouldBeM (DotfileSrcMissing (T.pack srcFile)) $ runWS $ computeDotfileDiff cfg (T.pack srcFile) (T.pack destFile)

    describe "computeDotfilePaths" $ do
      it "computes correct paths with default destDir" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Nothing  -- defaults to "~/"
              , files = []
              }
        let cfg = testDotfileConfig { src = "vimrc", dot = True }
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
        let cfg = testDotfileConfig { src = "bashrc", dot = True }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/bashrc"
        dest `shouldBe` "/home/user/.bashrc"

      it "computes correct paths with explicit dest" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Just "/home/user/"
              , files = []
              }
        let cfg = testDotfileConfig { src = "vim", dest = Just "custom-vim" }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/vim"
        dest `shouldBe` "/home/user/custom-vim"

      it "uses absolute dest path directly" $ do
        let dotfilesP = DotfilesP
              { srcDir = "/home/user/dotfiles"
              , destDir = Just "/home/user/"
              , files = []
              }
        let cfg = testDotfileConfig { src = "gitconfig", dest = Just "/etc/gitconfig", sort = Copy }
        (src, dest) <- runWS $ computeDotfilePaths dotfilesP cfg
        src `shouldBe` "/home/user/dotfiles/gitconfig"
        dest `shouldBe` "/etc/gitconfig"

    describe "applyDotfileFix" $ do
      it "creates symlink for DotfileMissing" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        runWS $ applyDotfileFix testDotfileConfig (T.pack srcFile) (T.pack destFile) DotfileMissing

        shouldBeM True $ doesPathExist destFile
        shouldBeM True $ pathIsSymbolicLink destFile

      it "creates copy for DotfileMissing with Copy mode" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"

        let cfg = testDotfileConfig { sort = Copy }
        runWS $ applyDotfileFix cfg (T.pack srcFile) (T.pack destFile) DotfileMissing

        shouldBeM True $ doesPathExist destFile
        shouldBeM False $ pathIsSymbolicLink destFile
        shouldBeM "content" $ readFile destFile

      it "creates a missing destination directory instead of silently failing" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        -- Destination lives under nested dirs that do not exist yet.
        let missingDir = destDir </> "does" </> "not" </> "exist"
        let destFile = missingDir </> "testfile"
        createTestFile srcFile "content"

        shouldBeM False $ doesPathExist missingDir
        runWS $ applyDotfileFix testDotfileConfig (T.pack srcFile) (T.pack destFile) DotfileMissing

        shouldBeM True $ doesPathExist destFile
        shouldBeM True $ pathIsSymbolicLink destFile

      it "removes broken symlink and creates new one for DotfileBrokenSymlink" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let oldTarget = srcDir </> "oldtarget"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "content"
        createTestFile oldTarget "old"
        createSymbolicLink oldTarget destFile
        removeFile oldTarget  -- Break the symlink

        runWS $ applyDotfileFix testDotfileConfig (T.pack srcFile) (T.pack destFile) DotfileBrokenSymlink

        shouldBeM True $ doesPathExist destFile
        shouldBeM True $ pathIsSymbolicLink destFile
        -- Verify it points to the right target now
        shouldBeM DotfileCorrect $ runWS $ computeDotfileDiff testDotfileConfig (T.pack srcFile) (T.pack destFile)

      it "backs up wrong file and creates new one for DotfileWrong" $ withTempSrcAndDest $ \srcDir destDir -> do
        let srcFile = srcDir </> "testfile"
        let destFile = destDir </> "testfile"
        createTestFile srcFile "correct content"
        createTestFile destFile "wrong content"

        runWS $ applyDotfileFix testDotfileConfig (T.pack srcFile) (T.pack destFile) DotfileWrong

        -- Original dest should now be correct
        shouldBeM True $ doesPathExist destFile
        shouldBeM True $ pathIsSymbolicLink destFile

        -- A backup file should exist
        backupFiles <- listDirectory destDir
        length backupFiles `shouldSatisfy` (> 1)  -- testfile + backup
