{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module WSHS.Configuration where

import WSHS.Properties.Git         (GitTrackHomeDirP)
import WSHS.Properties.Dotfiles    (DotfilesP)
import WSHS.Properties.Nix         (NixDaemonP)
import WSHS.Properties.HomeManager (HomeManagerP)
import WSHS.Properties.MacOS       (HomebrewBundleP)
import Data.Aeson.Types hiding (Parser, Options)
import GHC.Generics (Generic)
import Data.Text (Text)

data Property
  = GitHomeDir      GitTrackHomeDirP
  | Dotfiles        DotfilesP
  | NixDaemon       NixDaemonP
  | HomeManager     HomeManagerP
  | HomebrewBundle  HomebrewBundleP
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
  { configDir        :: Text
  , configRepoUrl    :: Text
  , configRepoOrigin :: Text
  , configRepoBranch :: Text
  , properties       :: [Property]
  } deriving (Generic, Show, FromJSON)
