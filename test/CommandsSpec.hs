{-# LANGUAGE OverloadedStrings #-}

module CommandsSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import Data.Either (isLeft)
import System.Environment (setEnv)

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

  describe "expandText" $ do
    it "expands variables and command substitutions" $ do
      setEnv "FUNSTATION_TEST_USER" "someone"
      shouldBeM "someone x\n" $ runWS $ expandText "$FUNSTATION_TEST_USER $(echo x)\n"

    it "keeps sudoers syntax, comments, globs and lines intact" $ do
      setEnv "FUNSTATION_TEST_USER" "someone"
      let input = "# a comment\n$FUNSTATION_TEST_USER ALL=(ALL) NOPASSWD: ALL\nDefaults   env_keep += \"*\"\n"
      shouldBeM "# a comment\nsomeone ALL=(ALL) NOPASSWD: ALL\nDefaults   env_keep += \"*\"\n" $
        runWS $ expandText input

    it "keeps an escaped \\$ literal" $
      shouldBeM "cost: $5\n" $ runWS $ expandText "cost: \\$5\n"

    it "preserves the absence of a trailing newline" $
      shouldBeM "no newline" $ runWS $ expandText "no newline"

    it "fails on an unset variable rather than expanding to nothing" $ do
      result <- runWSEither MacOS $ expandText "$FUNSTATION_DEFINITELY_UNSET_VAR\n"
      result `shouldSatisfy` isLeft
