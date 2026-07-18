{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.Homebrew where

import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Funstation.Properties.XCodeCLITools
import Shh (devNull, (&>))
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data HomebrewP = HomebrewP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HomebrewP where
  desc _ = "homebrew package manager for macOS"
  attrs _ = mempty
  checker _ = hasCmd' "brew"
  fixer _ = do
    result <- runCmd ["bash", "-c", "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"] (&> devNull)
    case result of
      Right _ -> putStrLn' "Homebrew installed successfully"
      Left err -> putStrLn' $ "Failed to install homebrew: " <> tshow err
  dependencies _ = return [(IsProp XCodeCLIToolsP)]
