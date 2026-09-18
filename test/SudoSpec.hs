{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}

module SudoSpec (spec) where

import Test.Hspec
import Funstation.Sudo
import Data.Text qualified as T
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Shh.Internal (exe, captureTrim, (|>))

import TestHelpers (shouldBeM, withFileMode)
import TestHelpers qualified

spec :: Spec
spec = do
  let withTempDir = TestHelpers.withTempDir "funstation-sudo-test"

  describe "needsSudoRead" $ do
    it "returns False for a user-owned readable file" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readable.txt"
      writeFile f "content"
      shouldBeM False $ needsSudoRead (T.pack f)

    it "returns False for a non-existent path" $ withTempDir $ \tmpDir -> do
      shouldBeM False $ needsSudoRead (T.pack (tmpDir </> "nonexistent"))

    it "returns True for a file with no read permission (chmod 000)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "unreadable.txt"
      writeFile f "content"
      withFileMode f 0o000 $
        shouldBeM True $ needsSudoRead (T.pack f)

  describe "needsSudo" $ do
    it "returns False for a user-owned writable file" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "writable.txt"
      writeFile f "content"
      shouldBeM False $ needsSudo (T.pack f)

    it "returns False for a non-existent path in a user-owned directory" $ withTempDir $ \tmpDir -> do
      shouldBeM False $ needsSudo (T.pack (tmpDir </> "nonexistent"))

    it "returns True for a read-only file (chmod 444)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "content"
      withFileMode f 0o444 $
        shouldBeM True $ needsSudo (T.pack f)

    it "returns True when the nearest existing ancestor is not writable" $ withTempDir $ \tmpDir -> do
      let locked = tmpDir </> "locked"
      createDirectory locked
      withFileMode locked 0o555 $
        shouldBeM True $ needsSudo (T.pack (locked </> "deep" </> "nested" </> "leaf"))

  describe "needsSudoEntry" $ do
    it "returns False for a path in a writable directory" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      shouldBeM False $ needsSudoEntry (T.pack f)

    it "returns False for a read-only file in a writable directory" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "content"
      withFileMode f 0o444 $ do
        shouldBeM True  $ needsSudo (T.pack f)       -- contents cannot be written
        shouldBeM False $ needsSudoEntry (T.pack f)  -- but it can still be renamed

    -- The /etc/nix/nix.conf shape: the file itself is writable, but it cannot
    -- be renamed because the directory holding it is not.
    it "returns True for a writable file in a read-only directory" $ withTempDir $ \tmpDir -> do
      let locked = tmpDir </> "locked"
      createDirectory locked
      let f = locked </> "writable.txt"
      writeFile f "content"
      withFileMode locked 0o555 $ do
        shouldBeM False $ needsSudo (T.pack f)       -- the file is writable
        shouldBeM True  $ needsSudoEntry (T.pack f)  -- but it cannot be renamed

    it "returns True for a new path in a read-only directory" $ withTempDir $ \tmpDir -> do
      let locked = tmpDir </> "locked"
      createDirectory locked
      withFileMode locked 0o555 $
        shouldBeM True $ needsSudoEntry (T.pack (locked </> "new.txt"))

  describe "needsSudoForAny" $ do
    it "returns False when every access is allowed" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      shouldBeM False $ needsSudoForAny [(ReadAccess, T.pack f), (EntryAccess, T.pack f)]

    it "returns False for no accesses at all" $
      shouldBeM False $ needsSudoForAny []

    -- `mv src dst` touches both ends: an out-of-reach destination escalates
    -- even when the source is the current user's own temp file.
    it "returns True when only the destination is not allowed" $ withTempDir $ \tmpDir -> do
      let src = tmpDir </> "src.txt"
      writeFile src "content"
      let locked = tmpDir </> "locked"
      createDirectory locked
      withFileMode locked 0o555 $
        shouldBeM True $ needsSudoForAny
          [(EntryAccess, T.pack src), (EntryAccess, T.pack (locked </> "dst.txt"))]

    it "returns True when only the source is not allowed" $ withTempDir $ \tmpDir -> do
      let locked = tmpDir </> "locked"
      createDirectory locked
      let src = locked </> "src.txt"
      writeFile src "content"
      withFileMode locked 0o555 $
        shouldBeM True $ needsSudoForAny
          [(EntryAccess, T.pack src), (EntryAccess, T.pack (tmpDir </> "dst.txt"))]

  describe "nearestExistingAncestor" $ do
    it "returns the path itself when it exists" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      shouldBeM f $ nearestExistingAncestor f

    it "returns the parent when only the leaf is missing" $ withTempDir $ \tmpDir ->
      shouldBeM tmpDir $ nearestExistingAncestor (tmpDir </> "missing")

    it "skips past several missing levels" $ withTempDir $ \tmpDir ->
      shouldBeM tmpDir $ nearestExistingAncestor (tmpDir </> "a" </> "b" </> "c")

    it "terminates at the root rather than looping" $
      shouldBeM "/" $ nearestExistingAncestor "/nonexistent-abc/def/ghi"

  describe "needsSudoFor" $ do
    it "ReadAccess dispatches to needsSudoRead" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      direct <- needsSudoRead (T.pack f)
      shouldBeM direct $ needsSudoFor ReadAccess (T.pack f)

    it "WriteAccess dispatches to needsSudo" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "file.txt"
      writeFile f "content"
      direct <- needsSudo (T.pack f)
      shouldBeM direct $ needsSudoFor WriteAccess (T.pack f)

    it "ReadAccess and WriteAccess can differ: read-only file needs write sudo but not read sudo" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "content"
      withFileMode f 0o444 $ do
        shouldBeM False $ needsSudoFor ReadAccess  (T.pack f)  -- 444 is readable by owner
        shouldBeM True  $ needsSudoFor WriteAccess (T.pack f)  -- 444 is not writable

  describe "mkPrivCmdFor (multi-path branch selection)" $ do
    it "uses the env prefix when every access is allowed" $ withTempDir $ \tmpDir -> do
      let src = tmpDir </> "src.txt"
      writeFile src "content"
      let dst = tmpDir </> "dst.txt"
      args <- mkPrivCmdFor "my-sudo" [(EntryAccess, T.pack src), (EntryAccess, T.pack dst)]
                ["mv", T.pack src, T.pack dst]
      take 1 args `shouldBe` ["env"]

    it "uses the sudo prefix when one end of a mv is not allowd" $ withTempDir $ \tmpDir -> do
      let src = tmpDir </> "src.txt"
      writeFile src "content"
      let locked = tmpDir </> "locked"
      createDirectory locked
      let dst = locked </> "dst.txt"
      withFileMode locked 0o555 $ do
        args <- mkPrivCmdFor "my-sudo" [(EntryAccess, T.pack src), (EntryAccess, T.pack dst)]
                  ["mv", T.pack src, T.pack dst]
        take 1 args `shouldBe` ["my-sudo"]

  describe "mkPrivCmd (permission-driven branch selection)" $ do
    it "forms cmd with env if sudo unneeded" $ withTempDir $ \tmpDir -> do
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "sudo" WriteAccess (T.pack tmpDir)
                ["bash", "-c", T.pack $ "echo env-branch > " <> outFile]
      _ <- exe (T.unpack <$> args)
      shouldBeM "env-branch\n" $ readFile outFile

    it "takes the sudo branch for a read-only file (injected as env for safety)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "readonly.txt"
      writeFile f "original"
      let outFile = tmpDir </> "out.txt"
      withFileMode f 0o444 $ do
        args <- mkPrivCmd "env" WriteAccess (T.pack f)
                  ["bash", "-c", T.pack $ "echo sudo-branch > " <> outFile]
        _ <- exe (T.unpack <$> args)
        shouldBeM "sudo-branch\n" $ readFile outFile

    it "ReadAccess: takes env branch for readable file, result is captured" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "src.txt"
      writeFile f "hello"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack f) ["cat", T.pack f]
      shouldBeM "hello" $ exe (T.unpack <$> args) |> captureTrim

    it "ReadAccess: takes sudo branch for unreadable file (injected as env)" $ withTempDir $ \tmpDir -> do
      let f = tmpDir </> "secret.txt"
      writeFile f "secret"
      let outFile = tmpDir </> "out.txt"
      withFileMode f 0o000 $ do
        args <- mkPrivCmd "env" ReadAccess (T.pack f)
                  ["bash", "-c", T.pack $ "echo read-sudo-branch > " <> outFile]
        _ <- exe (T.unpack <$> args)
        shouldBeM "read-sudo-branch\n" $ readFile outFile
