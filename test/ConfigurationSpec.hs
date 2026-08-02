{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ConfigurationSpec (spec) where

import Test.Hspec
import Text.RawString.QQ (r)
import Data.Yaml (decodeThrow)
import Data.ByteString (ByteString)
import Funstation
  ( Configuration(..)
  , Workstation(..)
  , NamedProperty(..)
  , Property(..)
  )

sampleYaml :: ByteString
sampleYaml = [r|
workstations:
  - name: glamdring
  - name: nixbox
    use: [home-dotfiles, bitwarden]
properties:
  - name: home-dotfiles
    type: GitHomeDir
    params:
      gitDir: .git-dir
      remoteUrl: git@example.com:me/dotfiles.git
      branch: master
  - type: BitwardenSecrets
    params:
      syncIntervalDays: 7
  - name: bitwarden
    type: BitwardenSecrets
    params:
      syncIntervalDays: 3
|]

spec :: Spec
spec = describe "ConfigurationSpec" $ do
  it "parses workstation names and optional use lists" $ do
    cfg <- decodeThrow sampleYaml :: IO Configuration
    map (.name) (workstations cfg) `shouldBe` ["glamdring", "nixbox"]
    map use (workstations cfg) `shouldBe` [Nothing, Just ["home-dotfiles", "bitwarden"]]

  it "parses optional names on registry entries" $ do
    cfg <- decodeThrow sampleYaml :: IO Configuration
    map (.name) (properties cfg) `shouldBe` [Just "home-dotfiles", Nothing, Just "bitwarden"]

  it "decodes the same object as a Property (type/params) alongside the name" $ do
    cfg <- decodeThrow sampleYaml :: IO Configuration
    case map property (properties cfg) of
      [GitHomeDir _, BitwardenSecrets _, BitwardenSecrets _] -> pure ()
      other -> expectationFailure $ "unexpected property shapes: " ++ show other
