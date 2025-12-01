{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

-- file Spec.hs
import Test.Hspec
import Control.Exception (evaluate)
import Text.RawString.QQ
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)
import WSHS.Types.Configuration
import Control.Monad (-- void,
                      forM_)




  -- - type: Dotfiles
  --   src-dir: dotfiles/
  --   files:
  --   - type: ln dot
  --     name: bashrc

  --   - type: ln dot
  --     name: gitconfig

  --   - type: ln dot
  --     name: zshrc

  --   - type: ln
  --     name: Brewfile

  --   - type: ln dot dir
  --     name: config/git

configText :: Text
configText = [r|
dotfilesRepoUrl: "https://github.com/joelmccracken/dotfiles"
dotfilesRepoOrigin: "git@github.com:joelmccracken/dotfiles.git"
workstationName: "aeglos"
properties:
- type: GitHomeDir
  gitDir: ".git-homedir"
|]


main :: IO ()
main = hspec $ do
  describe "configuration" $ do
    it "extracts configuration for properties" $ do
      cfg <- decodeThrow (encodeUtf8 configText) :: IO Configuration
      forM_ cfg.properties $ \prop -> do
        print prop

      -- withPropCfg "git-home-dir" Nothing (\x -> pure $ Just ("" :: Text))

      'a' `shouldBe` 'a'
