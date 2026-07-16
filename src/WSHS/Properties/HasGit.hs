{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WSHS.Properties.HasGit where

import Control.Monad.Except (throwError)
import WSHS.Types
import WSHS.Commands
import WSHS.Properties.Homebrew
import WSHS.Properties.AptUpdate
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HasGitP where
  desc _ = "has command `git` installed"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  dependencies _ = do
    os <- detectOS
    case os of
      MacOS -> return [(IsProp HomebrewP)]
      Debian -> return [(IsProp AptUpdateP)]
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- detectOS
    case os of
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
      MacOS -> do
        result <- brewInstall "git"
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> throwError $ WSFailure $ "Failed to install git: " <> tshow err
      Debian -> do
        result <- aptInstall "git"
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> throwError $ WSFailure $ "Failed to install git: " <> tshow err
