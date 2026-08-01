{-# LANGUAGE OverloadedStrings #-}

module Properties.HasGitSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import Funstation.Types (OS(..), dependencies, fixer)
import Funstation.Properties.HasGit (HasGitP(..))
import TestHelpers (runWSWithOS, runWSEither)

spec :: Spec
spec = describe "HasGit on NixOS" $ do
  it "pulls in no package-manager dependency" $ do
    deps <- runWSWithOS NixOS (dependencies HasGitP)
    length deps `shouldBe` 0

  it "fixer errors, directing the user to configuration.nix" $ do
    result <- runWSEither NixOS (fixer HasGitP)
    result `shouldSatisfy` isLeft
