{-# LANGUAGE DeriveAnyClass    #-}

module WSHS.Types.Configuration where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  }
  deriving (Generic, Show, FromJSON)
