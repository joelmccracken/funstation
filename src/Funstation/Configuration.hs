{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Funstation.Configuration where

import Funstation.Properties.GitHomeDir  (GitHomeDirP)
import Funstation.Properties.GitClone    (GitCloneP)
import Funstation.Properties.Dotfiles    (DotfilesP)
import Funstation.Properties.Nix         (NixDaemonP)
import Funstation.Properties.HomeManager (HomeManagerP)
import Funstation.Properties.HomebrewBundle (HomebrewBundleP)
import Funstation.Properties.BitwardenSecrets (BitwardenSecretsP)
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
