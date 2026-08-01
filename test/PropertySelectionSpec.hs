{-# LANGUAGE OverloadedStrings #-}

module PropertySelectionSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import Funstation
  ( resolvePropertiesFor
  , Configuration(..)
  , Workstation(..)
  , NamedProperty(..)
  , Property(..)
  )
import Funstation.Properties.BitwardenSecrets (BitwardenSecretsP(..))

-- Use BitwardenSecrets entries whose syncIntervalDays acts as a distinguishable label,
-- so we can assert both selection and ordering.
bw :: Int -> Property
bw n = BitwardenSecrets (BitwardenSecretsP n)

label :: Property -> Int
label (BitwardenSecrets p) = syncIntervalDays p
label _ = -1

cfg :: Configuration
cfg = Configuration
  { workstations =
      [ Workstation "all-default" Nothing
      , Workstation "subset" (Just ["b", "a"])
      , Workstation "bad" (Just ["nope"])
      ]
  , properties =
      [ NamedProperty (Just "a") (bw 1)
      , NamedProperty (Just "b") (bw 2)
      , NamedProperty Nothing    (bw 3)
      ]
  }

spec :: Spec
spec = describe "resolvePropertiesFor" $ do
  it "runs the whole registry in file order when the workstation has no use list" $
    fmap (map label) (resolvePropertiesFor cfg "all-default") `shouldBe` Right [1, 2, 3]

  it "runs exactly the named entries, in use-list order" $
    fmap (map label) (resolvePropertiesFor cfg "subset") `shouldBe` Right [2, 1]

  it "errors on an unknown property name" $
    resolvePropertiesFor cfg "bad" `shouldSatisfy` isLeft

  it "falls back to run-all for a name that is not a declared workstation" $
    fmap (map label) (resolvePropertiesFor cfg "workstation") `shouldBe` Right [1, 2, 3]
