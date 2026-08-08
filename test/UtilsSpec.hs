{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module UtilsSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import System.Directory
import System.FilePath ((</>))
import Shh.Internal (exe, devNull, (&>), captureTrim, (|>), tryFailure)
import Data.Either (isRight, isLeft)
import Data.Maybe (isJust)

import Funstation hiding (main, failLeft)
import Funstation.Proc
import TestHelpers hiding (withTempDir)
import TestHelpers qualified

spec :: Spec
spec = do
  let withTempDir = TestHelpers.withTempDir "funstation-test"

  describe "fileContentsCheck" $ do
    it "returns True when file exists with matching contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "test content\nline 2\n"
      writeFile testFile content
      shouldBeM True $ runWS $ fileContentsCheck (T.pack testFile) (T.pack content)

    it "returns False when file exists with different contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      shouldBeM False $ runWS $ fileContentsCheck (T.pack testFile) "different content"

    it "returns False when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "nonexistent"
      shouldBeM False $ runWS $ fileContentsCheck (T.pack testFile) "some content"

    it "returns True for empty file with empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      shouldBeM True $ runWS $ fileContentsCheck (T.pack testFile) ""

    it "returns False for empty file with non-empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      shouldBeM False $ runWS $ fileContentsCheck (T.pack testFile) "some content"

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let content = "line 1\nline 2\nline 3\n"
      writeFile testFile content
      shouldBeM True $ runWS $ fileContentsCheck (T.pack testFile) (T.pack content)

  describe "fileContentsFix" $ do
    it "returns Nothing when file already has correct contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "correct content"
      writeFile testFile content
      shouldBeM Nothing $ runWS $ fileContentsFix (T.pack testFile) (T.pack content)
      -- Verify no backup was created
      files <- listDirectory tmpDir
      length files `shouldBe` 1

    it "returns Just backupPath when file exists with wrong contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      shouldSatisfyM (\case Just path -> not (T.null path); Nothing -> False) $
        runWS $ fileContentsFix (T.pack testFile) "new content"
      -- Verify file was updated
      shouldBeM "new content" $ readFile testFile
      -- Verify backup exists
      files <- listDirectory tmpDir
      length files `shouldBe` 2  -- original + backup

    it "returns Just empty string when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "newfile"
      shouldBeM (Just "") $ runWS $ fileContentsFix (T.pack testFile) "new content"
      -- Verify file was created
      shouldBeM True $ doesPathExist testFile
      shouldBeM "new content" $ readFile testFile

    it "backup file contains original contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let originalContent = "original content"
      writeFile testFile originalContent
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      case result of
        Just backupPath -> shouldBeM originalContent $ readFile (T.unpack backupPath)
        Nothing -> expectationFailure "Expected Just backupPath"

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let originalContent = "line 1\nline 2\n"
      let newContent = "new line 1\nnew line 2\nnew line 3\n"
      writeFile testFile originalContent
      shouldSatisfyM isJust $ runWS $ fileContentsFix (T.pack testFile) (T.pack newContent)
      -- Verify new content
      shouldBeM newContent $ readFile testFile

  describe "mkPrivCmd" $ do
    it "uses env prefix when path is user-owned (no sudo needed)" $ withTempDir $ \tmpDir -> do
      -- Path is user-owned → needsSudo returns False → exe ("env" : args)
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "sudo" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo hello > " <> outFile]
      _ <- exe (T.unpack <$> args)
      shouldBeM "hello\n" $ readFile outFile

    it "uses injected sudo command when needs check returns True (injected as env)" $ withTempDir $ \tmpDir -> do
      -- inject "env" as the sudo command so the command succeeds even if sudo branch is taken
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "env" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo injected > " <> outFile]
      _ <- exe (T.unpack <$> args)
      shouldBeM "injected\n" $ readFile outFile

    it "returned Cmd can be chained with |> to capture output" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "captured content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      shouldBeM "captured content" $ exe (T.unpack <$> args) |> captureTrim

    it "returned Cmd can be chained with &> devNull" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "some content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      shouldSatisfyM isRight $ tryFailure $ exe (T.unpack <$> args) &> devNull

  describe "privCmd" $ do
    it "runs WriteAccess command on user-owned path without sudo" $ withTempDir $ \tmpDir -> do
      let outFile = tmpDir </> "out.txt"
      shouldSatisfyM isRight $ runWS $ privCmd WriteAccess (T.pack tmpDir)
                          ["bash", "-c", T.pack $ "echo write-ok > " <> outFile]
      shouldBeM "write-ok\n" $ readFile outFile

    it "reads sudoCmd from Settings (injected as env) for write path" $ withTempDir $ \tmpDir -> do
      -- sudoCmd = "env" means even if sudo were needed, env is used — command succeeds
      let outFile = tmpDir </> "out.txt"
      shouldSatisfyM isRight $ runWSWith "env" $ privCmd WriteAccess (T.pack tmpDir)
                                    ["bash", "-c", T.pack $ "echo env-sudo > " <> outFile]
      shouldBeM "env-sudo\n" $ readFile outFile

    it "runs ReadAccess command on user-owned file" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "read-ok"
      shouldSatisfyM isRight $ runWS $ privCmd ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]

    it "fails when the command itself fails" $ withTempDir $ \tmpDir -> do
      shouldSatisfyM isLeft $ runWS $ privCmd WriteAccess (T.pack tmpDir) ["false"]
