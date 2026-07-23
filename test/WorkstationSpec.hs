{-# LANGUAGE OverloadedStrings #-}

module WorkstationSpec (spec) where

import Test.Hspec
import Data.Either
import Data.Text (Text)
import Funstation
  ( knownWorkstations
  , resolveWorkstationName
  , WorkstationSource(..)
  , Configuration(..)
  , Workstation(..)
  )

-- | Build a Configuration declaring the given workstation names (no properties).
cfg :: [Text] -> Configuration
cfg names = Configuration { workstations = map Workstation names, properties = [] }

spec :: Spec
spec = do
  describe "knownWorkstations" $ do
    it "defaults to [\"workstation\"] when the config declares none" $
      knownWorkstations [] `shouldBe` ["workstation"]

    it "uses the declared names when present" $
      knownWorkstations ["a", "b"] `shouldBe` ["a", "b"]

  describe "resolveWorkstationName" $ do
    it "uses a valid CLI name, tagged FromCLI" $
      resolveWorkstationName (cfg ["glamdring", "narsil"]) (Just "narsil") Nothing
        `shouldBe` Right (FromCLI "narsil")

    it "rejects a CLI name that is not a known workstation" $
      resolveWorkstationName (cfg ["glamdring"]) (Just "bogus") Nothing
        `shouldSatisfy` isLeft

    it "uses a saved state name, tagged FromStateFile" $
      resolveWorkstationName (cfg ["glamdring", "narsil"]) Nothing (Just "narsil")
        `shouldBe` Right (FromStateFile "narsil")

    it "rejects a saved state name that is no longer known" $
      resolveWorkstationName (cfg ["glamdring"]) Nothing (Just "stale")
        `shouldSatisfy` isLeft

    it "prefers the CLI name over saved state" $
      resolveWorkstationName (cfg ["glamdring", "narsil"]) (Just "glamdring") (Just "narsil")
        `shouldBe` Right (FromCLI "glamdring")

    it "uses the single declared workstation, tagged FromDefault" $
      resolveWorkstationName (cfg ["glamdring"]) Nothing Nothing
        `shouldBe` Right (FromDefault "glamdring")

    it "defaults to \"workstation\" when the config declares none" $
      resolveWorkstationName (cfg []) Nothing Nothing
        `shouldBe` Right (FromDefault "workstation")

    it "errors when several are declared and no name is given" $
      resolveWorkstationName (cfg ["glamdring", "narsil"]) Nothing Nothing
        `shouldSatisfy` isLeft
