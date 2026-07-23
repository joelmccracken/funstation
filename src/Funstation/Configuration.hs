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
import Data.Text (Text)
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

data Workstation = Workstation
  { workstationName :: Text
  } deriving (Generic, Show, FromJSON)

data Configuration = Configuration
  { workstations :: [Workstation]
  , properties :: [Property]
  } deriving (Generic, Show)

-- Hand-written so a missing `workstations` key defaults to [] instead of
-- failing to parse (existing configs and tests may omit it).
instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
    Configuration
      <$> o .:? "workstations" .!= []
      <*> o .:  "properties"
