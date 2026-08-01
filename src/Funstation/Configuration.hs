{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Funstation.Configuration where

import Funstation.Properties.GitHomeDir  (GitHomeDirP)
import Funstation.Properties.GitClone    (GitCloneP)
import Funstation.Properties.Dotfiles    (DotfilesP)
import Funstation.Properties.NixDaemon         (NixDaemonP)
import Funstation.Properties.HomeManager (HomeManagerP)
import Funstation.Properties.HomebrewBundle (HomebrewBundleP)
import Funstation.Properties.BitwardenSecrets (BitwardenSecretsP)
import Funstation.Types (WorkstationName, PropertyName)
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

-- | A registry entry: a 'Property' with an optional name. The name is a
-- reference handle (used by 'Workstation.use')
-- Unnamed entries run under the run-all default (a workstation with no 'use').
data NamedProperty = NamedProperty
  { name     :: Maybe PropertyName
  , property :: Property
  } deriving (Show, Generic)

-- Hand-written so the optional @name@ sits at the same object level as @type@/@params@:
-- read @name@ off the object, then decode the same object as a 'Property'
instance FromJSON NamedProperty where
  parseJSON = withObject "NamedProperty" $ \o ->
    NamedProperty <$> o .:? "name" <*> parseJSON (Object o)

data Workstation = Workstation
  { workstationName :: WorkstationName
  , use :: Maybe [PropertyName]  -- ^ Names of registry entries to run, in order; 'Nothing' runs all
  } deriving (Generic, Show, FromJSON)

data Configuration = Configuration
  { workstations :: [Workstation]
  , properties :: [NamedProperty]
  } deriving (Generic, Show)

-- Hand-written so a missing `workstations` key defaults to [] instead of
-- failing to parse (existing configs and tests may omit it).
instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
    Configuration
      <$> o .:? "workstations" .!= []
      <*> o .:  "properties"
