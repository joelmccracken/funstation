{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.Debian where

import WSHS.Types
import WSHS.Commands
import Shh (exe, devNull, (&>))
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop AptUpdateP where
  desc _ = "apt package lists updated"
  attrs _ = mempty
  checker _ = return False  -- Always run update to be safe
  fixer _ = do
    args' <- mkWSCmd ["sudo", "apt-get", "update"]
    result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
    case result of
      Right _ -> putStrLn' "apt package sets updated successfully"
      Left err -> putStrLn' $ "Failed to update apt: " <> tshow err
  dependencies _ = return []
