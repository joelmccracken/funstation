{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TemplateHaskell #-}
module WSHS.Types.Configuration where

import Data.Text (Text)
import Data.Yaml (FromJSON, Value)
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics (Generic)

data GitHomeDirPS = GitHomeDirPS { gitDir :: Text }
 deriving Show

$(deriveJSON defaultOptions ''GitHomeDirPS)

data PropSettings
  = GitHomeDir GitHomeDirPS
  deriving (Show)

deriveJSON defaultOptions{ sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "params" }
                           } ''PropSettings

data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  , properties :: [PropSettings]
  }
  deriving (Generic, Show, FromJSON)
