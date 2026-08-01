{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Funstation.Properties.HasGit where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Funstation.Types
import Funstation.Commands
import Funstation.Properties.Homebrew
import Funstation.Properties.AptUpdate
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HasGitP where
  desc _ = "has command `git` installed"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  dependencies _ = do
    os <- asks (.os)
    case os of
      MacOS -> return [(IsProp HomebrewP)]
      Debian -> return [(IsProp AptUpdateP)]
      -- On NixOS git is expected to be provided declaratively; no package-manager
      -- dependency to pull in.
      NixOS -> return []
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- asks (.os)
    case os of
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
      NixOS -> throwError $ WSFailure "git not found on NixOS; declare it in configuration.nix (environment.systemPackages)"
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
