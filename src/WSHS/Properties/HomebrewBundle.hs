{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.HomebrewBundle where

import Control.Monad.Except (throwError)
import WSHS.Types
import WSHS.Commands
import WSHS.Proc
import WSHS.Properties.Homebrew
import Shh (exe, devNull, (&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Either (isRight)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map

data HomebrewBundleP = HomebrewBundleP
  { brewfile :: Text   -- ^ path to the Brewfile
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HomebrewBundleP where
  desc _ = "homebrew bundle"
  attrs p = Map.fromList [("brewfile", p.brewfile)]
  checker p = do
    brewInstalled <- hasCmd' "brew"
    if not brewInstalled
      then return False
      else isRight <$> cmd (exe "brew" "bundle" "check" "--no-upgrade" ("--file=" <> T.unpack p.brewfile) &> devNull)
  fixer p = do
    result <- runCmd ["brew", "bundle", "install", "--file=" <> p.brewfile] id
    case result of
      Right _ -> putStrLn' "Homebrew bundle installed."
      Left err -> throwError $ WSFailure $ "brew bundle install failed: " <> tshow err
  dependencies _ = return [IsProp HomebrewP]
