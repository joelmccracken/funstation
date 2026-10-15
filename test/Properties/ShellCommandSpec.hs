{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Properties.ShellCommandSpec (spec) where

import Test.Hspec
import Text.RawString.QQ (r)
import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Data.Yaml (decodeThrow)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Funstation.Types (OS(..), IsProp(..), checker, fixer)
import Funstation.Configuration (Property(..))
import Funstation.Properties.ShellCommand
import TestHelpers (runWSWithOS, runWSEither, withTempDir)

-- | A property with no label and verification left on.
mkProp :: T.Text -> T.Text -> ShellCommandP
mkProp c f = ShellCommandP Nothing c f True

spec :: Spec
spec = do
  describe "checker" $ do
    it "is satisfied when the command exits 0" $ do
      result <- runWSWithOS MacOS $ checker (mkProp "true" "true")
      result `shouldBe` True

    it "is unsatisfied when the command exits non-zero" $ do
      result <- runWSWithOS MacOS $ checker (mkProp "exit 1" "true")
      result `shouldBe` False

  describe "fixer" $ do
    it "succeeds when the command exits 0 and the checker then passes" $ do
      result <- runWSEither MacOS $ fixer (mkProp "true" "true")
      result `shouldSatisfy` isRight

    it "fails when the command exits non-zero" $ do
      result <- runWSEither MacOS $ fixer (mkProp "exit 1" "exit 1")
      result `shouldSatisfy` isLeft

    it "fails when the command exits 0 but the checker still does not pass" $ do
      result <- runWSEither MacOS $ fixer (mkProp "false" "true")
      result `shouldSatisfy` isLeft

    it "skips that verification when verify is False" $ do
      let p = (mkProp "false" "true") { verify = False }
      result <- runWSEither MacOS $ fixer p
      result `shouldSatisfy` isRight

  describe "check then fix" $
    it "runs the fixer's effect, which the checker then vaildates" $
      withTempDir "funstation-shellcommand-test" $ \tmpDir -> do
        let marker = tmpDir </> "marker"
            p = mkProp
                  ("test -f " <> T.pack marker)
                  ("touch " <> T.pack marker)

        before' <- runWSWithOS MacOS $ checker p
        before' `shouldBe` False

        result <- runWSEither MacOS $ fixer p
        result `shouldSatisfy` isRight

        doesFileExist marker `shouldReturn` True
        after' <- runWSWithOS MacOS $ checker p
        after' `shouldBe` True

  describe "identity" $
    it "distinguishes properties with different commands" $ do
      let a = IsProp $ mkProp "true" "true"
          b = IsProp $ mkProp "true" "false"
      a == b `shouldBe` False

  describe "configuration" $ do
    it "parses a ShellCommand property" $ do
      prop <- decodeThrow [r|
type: ShellCommand
params:
  desc: touch id for sudo
  checker: grep -q pam_tid /etc/pam.d/sudo
  fixer: echo fixing
|]
      case prop of
        ShellCommand sp ->
          sp `shouldBe` ShellCommandP
            (Just "touch id for sudo")
            "grep -q pam_tid /etc/pam.d/sudo"
            "echo fixing"
            True
        other -> expectationFailure $ "expected ShellCommand, got " <> show other

    it "defaults verify to True and desc to Nothing" $ do
      prop <- decodeThrow [r|
type: ShellCommand
params:
  checker: "true"
  fixer: "true"
|]
      case prop of
        ShellCommand sp -> do
          verify sp `shouldBe` True
          label sp `shouldBe` Nothing
        other -> expectationFailure $ "expected ShellCommand, got " <> show other

    it "reads verify when it is given" $ do
      prop <- decodeThrow [r|
type: ShellCommand
params:
  checker: "true"
  fixer: "true"
  verify: false
|]
      case prop of
        ShellCommand sp -> verify sp `shouldBe` False
        other -> expectationFailure $ "expected ShellCommand, got " <> show other
