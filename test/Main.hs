{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Test.Hspec

import qualified SudoSpec
import qualified WorkstationSpec
import qualified UtilsSpec
import qualified Properties.GitHomeDirSpec
import qualified Properties.GitCloneSpec
import qualified Properties.DotfilesSpec

main :: IO ()
main = hspec $ do
  UtilsSpec.spec
  SudoSpec.spec
  WorkstationSpec.spec
  Properties.GitHomeDirSpec.spec
  Properties.GitCloneSpec.spec
  Properties.DotfilesSpec.spec
