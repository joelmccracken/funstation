{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module WSHS.Configuration where

import WSHS.Properties.GitHomeDir  (GitHomeDirP)
import WSHS.Properties.GitClone    (GitCloneP)
import WSHS.Properties.Dotfiles    (DotfilesP)
import WSHS.Properties.Nix         (NixDaemonP)
import WSHS.Properties.HomeManager (HomeManagerP)
import WSHS.Properties.HomebrewBundle (HomebrewBundleP)
import WSHS.Properties.BitwardenSecrets (BitwardenSecretsP)
import Data.Aeson.Types hiding (Parser, Options)
import GHC.Generics (Generic)

data Property
  = GitHomeDir      GitHomeDirP
  | GitClone        GitCloneP
  | Dotfiles        DotfilesP
  | NixDaemon       NixDaemonP
  | HomeManager     HomeManagerP
  | HomebrewBundle      HomebrewBundleP
  | BitwardenSecrets    BitwardenSecretsP
  deriving (Show, Generic)

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions { sumEncoding =
                                                    TaggedObject
                                                    { tagFieldName = "type"
                                                    , contentsFieldName = "params"
                                                    }
                                                }

instance FromJSON Property where
  parseJSON = genericParseJSON defaultOptions { sumEncoding =
                                                    TaggedObject
                                                    { tagFieldName = "type"
                                                    , contentsFieldName = "params"
                                                    }
                                                }

data Configuration = Configuration
  { properties :: [Property]
  } deriving (Generic, Show, FromJSON)
