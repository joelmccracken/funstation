{-# LANGUAGE OverloadedStrings #-}

module CommandsSpec (spec) where

import Test.Hspec
import Data.Text qualified as T

import Funstation hiding (main, failLeft)
import TestHelpers

spec :: Spec
spec = do
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
      shouldBeM "/usr/local/bin" $ runWS $ expandPath "/usr/local/bin"

    it "expands $HOME to home directory" $ do
      result <- runWS $ expandPath "$HOME"
      result `shouldSatisfy` T.isPrefixOf "/"
      result `shouldSatisfy` (not . T.isInfixOf "$")

    it "expands $HOME in path" $ do
      result <- runWS $ expandPath "$HOME/foo/bar"
      result `shouldSatisfy` T.isPrefixOf "/"
      result `shouldSatisfy` T.isSuffixOf "/foo/bar"
