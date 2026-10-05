{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Properties.SudoersSpec (spec) where

import Test.Hspec
import Text.RawString.QQ (r)
import Data.Either (isLeft, isRight)
import Data.Yaml (decodeThrow)
import System.Directory (findExecutable)
import System.FilePath ((</>))
import Funstation.Types (OS(..), checker, fixer)
import Funstation.Configuration (Property(..))
import Funstation.Properties.Sudoers
import TestHelpers (runWSWithOS, runWSEither, withTempDir)

-- | Skip a test when visudo is unavailable (e.g. in the nix build sandbox).
withVisudo :: IO () -> IO ()
withVisudo action = do
  found <- findExecutable "visudo"
  maybe (pendingWith "visudo not on PATH") (const action) found

spec :: Spec
spec = do
  describe "validateSudoersFilename" $ do
    it "accepts names sudo will read" $ do
      validateSudoersFilename "funstation" `shouldSatisfy` isRight
      validateSudoersFilename "zz-joel" `shouldSatisfy` isRight

    it "rejects names containing '.', which sudo ignores" $
      validateSudoersFilename "foo.conf" `shouldSatisfy` isLeft

    it "rejects names ending in '~', which sudo ignores" $
      validateSudoersFilename "foo~" `shouldSatisfy` isLeft

    it "rejects empty names and names with '/'" $ do
      validateSudoersFilename "" `shouldSatisfy` isLeft
      validateSudoersFilename "../sudoers" `shouldSatisfy` isLeft

  describe "sudoersPath" $ do
    it "defaults to /etc/sudoers.d/funstation" $ do
      path <- runWSWithOS MacOS $ sudoersPath (SudoersP Nothing "")
      path `shouldBe` "/etc/sudoers.d/funstation"

    it "fails on a filename sudo would ignore" $ do
      result <- runWSEither MacOS $ sudoersPath (SudoersP (Just "funstation.conf") "")
      result `shouldSatisfy` isLeft

  describe "visudoCheck" $ do
    let withTmp = withTempDir "funstation-sudoers-test"

    it "accepts valid sudoers syntax" $ withVisudo $ withTmp $ \tmpDir -> do
      let f = tmpDir </> "good"
      writeFile f "someone ALL=(ALL) NOPASSWD: ALL\n"
      result <- runWSWithOS MacOS $ visudoCheck f
      result `shouldBe` True

    it "rejects invalid sudoers syntax" $ withVisudo $ withTmp $ \tmpDir -> do
      let f = tmpDir </> "bad"
      writeFile f "someone ALL=(ALL\n"
      result <- runWSWithOS MacOS $ visudoCheck f
      result `shouldBe` False

  describe "on NixOS" $ do
    let p = SudoersP Nothing "$USER ALL=(ALL) NOPASSWD: ALL\n"

    it "checker is always satisfied" $ do
      result <- runWSWithOS NixOS (checker p)
      result `shouldBe` True

    it "fixer errors" $ do
      result <- runWSEither NixOS (fixer p)
      result `shouldSatisfy` isLeft

  describe "configuration" $
    it "parses a Sudoers property" $ do
      prop <- decodeThrow [r|
type: Sudoers
params:
  filename: funstation
  content: |
    $USER ALL=(ALL) NOPASSWD: ALL
|]
      case prop of
        Sudoers sp -> sp `shouldBe` SudoersP (Just "funstation") "$USER ALL=(ALL) NOPASSWD: ALL\n"
        other -> expectationFailure $ "expected Sudoers, got " <> show other
