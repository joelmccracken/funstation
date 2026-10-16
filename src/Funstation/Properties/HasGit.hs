{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Funstation.Properties.HasGit where

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
      Unknown -> wsFail "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- asks (.os)
    case os of
      Unknown -> wsFail "error: Unknown OS, unable to install git"
      NixOS -> wsFail "git not found on NixOS; declare it in configuration.nix (environment.systemPackages)"
      MacOS -> do
        orFail "Failed to install git" $ brewInstall "git"
        putStrLn' "Git installed successfully"
      Debian -> do
        orFail "Failed to install git" $ aptInstall "git"
        putStrLn' "Git installed successfully"
