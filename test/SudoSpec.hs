{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}

module SudoSpec (spec) where

import Test.Hspec
import WSHS.Sudo
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import Shh.Internal (exe, captureTrim, (|>))
import Control.Exception (bracket_)

-- | Temporarily set a file's mode, restoring the original after the action.
-- Ensures cleanup can proceed even if the action throws.
withFileMode :: FilePath -> Int -> IO a -> IO a
withFileMode path newMode action =
  bracket_
    (setFileMode path (fromIntegral newMode))
    (setFileMode path 0o644)
    action

spec :: Spec
spec = do
  let withTempDir fn = withSystemTempDirectory "wshs-sudo-test" $ \d -> fn d

  describe "needsSudoRead" $ do
    it "returns False for a user-owned readable file" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readable.txt"
      writeFile f "content"
      result <- needsSudoRead (T.pack f)
      result `shouldBe` False

    it "returns False for a non-existent path" $ withTempDir $ \tmpDir -> do
      result <- needsSudoRead (T.pack (tmpDir </> "nonexistent"))
      result `shouldBe` False

    it "returns True for a file with no read permission (chmod 000)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "unreadable.txt"
      writeFile f "content"
      withFileMode f 0o000 $ do
        result <- needsSudoRead (T.pack f)
        result `shouldBe` True

  describe "needsSudo" $ do
    it "returns False for a user-owned writable file" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "writable.txt"
      writeFile f "content"
      result <- needsSudo (T.pack f)
      result `shouldBe` False

    it "returns False for a non-existent path in a user-owned directory" $ withTempDir $ \tmpDir -> do
      result <- needsSudo (T.pack (tmpDir </> "nonexistent"))
      result `shouldBe` False

    it "returns True for a read-only file (chmod 444)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "content"
      withFileMode f 0o444 $ do
        result <- needsSudo (T.pack f)
        result `shouldBe` True

  describe "needsSudoFor" $ do
    it "ReadAccess dispatches to needsSudoRead" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      viaDispatch <- needsSudoFor ReadAccess (T.pack f)
      direct      <- needsSudoRead (T.pack f)
      viaDispatch `shouldBe` direct

    it "WriteAccess dispatches to needsSudo" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      viaDispatch <- needsSudoFor WriteAccess (T.pack f)
      direct      <- needsSudo (T.pack f)
      viaDispatch `shouldBe` direct

    it "ReadAccess and WriteAccess can differ: read-only file needs write sudo but not read sudo" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "content"
      withFileMode f 0o444 $ do
        needsReadSudo  <- needsSudoFor ReadAccess  (T.pack f)
        needsWriteSudo <- needsSudoFor WriteAccess (T.pack f)
        needsReadSudo  `shouldBe` False  -- 444 is readable by owner
        needsWriteSudo `shouldBe` True   -- 444 is not writable

  describe "mkPrivCmd (permission-driven branch selection)" $ do
    it "takes the env branch for a user-owned writable directory" $ withTempDir $ \tmpDir -> do
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "sudo" WriteAccess (T.pack tmpDir)
                ["bash", "-c", T.pack $ "echo env-branch > " <> outFile]
      _ <- exe (T.unpack <$> args)
      content <- readFile outFile
      content `shouldBe` "env-branch\n"

    it "takes the sudo branch for a read-only file (injected as env for safety)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "original"
      let outFile = tmpDir </> "out.txt"
      withFileMode f 0o444 $ do
        args <- mkPrivCmd "env" WriteAccess (T.pack f)
                  ["bash", "-c", T.pack $ "echo sudo-branch > " <> outFile]
        _ <- exe (T.unpack <$> args)
        content <- readFile outFile
        content `shouldBe` "sudo-branch\n"

    it "ReadAccess: takes env branch for readable file, result is captured" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "src.txt"
      writeFile f "hello"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack f) ["cat", T.pack f]
      result <- exe (T.unpack <$> args) |> captureTrim
      result `shouldBe` "hello"

    it "ReadAccess: takes sudo branch for unreadable file (injected as env)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "secret.txt"
      writeFile f "secret"
      let outFile = tmpDir </> "out.txt"
      withFileMode f 0o000 $ do
        args <- mkPrivCmd "env" ReadAccess (T.pack f)
                  ["bash", "-c", T.pack $ "echo read-sudo-branch > " <> outFile]
        _ <- exe (T.unpack <$> args)
        content <- readFile outFile
        content `shouldBe` "read-sudo-branch\n"
