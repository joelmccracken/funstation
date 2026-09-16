{-# LANGUAGE OverloadedStrings #-}

module StateSpec (spec) where

import Test.Hspec
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Funstation.State
import TestHelpers (shouldBeM)
import TestHelpers qualified

spec :: Spec
spec = do
  let withTempDir = TestHelpers.withTempDir "funstation-state-test"

  describe "stateDir" $
    it "is ~/.local/state/funstation" $
      stateDir "/home/user" `shouldBe` "/home/user/.local/state/funstation"

  describe "stateFile" $ do
    it "places a file at the root of the state dir" $
      stateFile "/home/user" "workstationName"
        `shouldBe` "/home/user/.local/state/funstation/workstationName"

    it "places a file in a per-property subdirectory" $
      stateFile "/home/user" ("apt-update" </> "last-run-ts")
        `shouldBe` "/home/user/.local/state/funstation/apt-update/last-run-ts"

  describe "writeStateFile" $ do
    it "creates the state directory if it does not exist" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir ("some-prop" </> "value")
      shouldBeM False $ doesFileExist path
      writeStateFile path "hello"
      shouldBeM True $ doesFileExist path

    it "round-trips through readStateFile" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "value"
      writeStateFile path "hello"
      shouldBeM (Just "hello") $ readStateFile path

    it "writes a trailing newline" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "value"
      writeStateFile path "hello"
      shouldBeM "hello\n" $ readFile path

    it "overwrites an existing value" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "value"
      writeStateFile path "first"
      writeStateFile path "second"
      shouldBeM (Just "second") $ readStateFile path

  describe "readStateFile" $
    it "is Nothing when the file has never been written" $ withTempDir $ \tmpDir ->
      shouldBeM Nothing $ readStateFile (stateFile tmpDir "absent")

  describe "readTimestamp" $ do
    it "round-trips a written timestamp" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "ts"
      writeTimestamp path
      recorded <- readTimestamp path
      recorded `shouldSatisfy` (> 1700000000)

    it "is 0 when nothing has been recorded" $ withTempDir $ \tmpDir ->
      shouldBeM 0 $ readTimestamp (stateFile tmpDir "ts")

    it "is 0 for a corrupt state file rather than crashing" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "ts"
      writeStateFile path "not a number"
      shouldBeM 0 $ readTimestamp path

    it "is 0 for an empty state file" $ withTempDir $ \tmpDir -> do
      let path = stateFile tmpDir "ts"
      writeStateFile path ""
      shouldBeM 0 $ readTimestamp path
