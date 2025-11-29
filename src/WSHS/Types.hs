{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module WSHS.Types where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import WSHS.Types.Configuration

data OS = MacOS | Debian | Unknown
  deriving (Show, Eq)

data Property m = Property
  { name :: Text
  , attrs :: Map.Map Text Text
  , checker :: m Bool
  , fixer :: m ()
  , dependencies :: m [Property m]
  }

instance Eq (Property m) where
  a == b = a.name == b.name && a.attrs == b.attrs

instance Ord (Property m) where
  a `compare` b =
    case a.name `compare` b.name of
      EQ ->  a.attrs `compare` b.attrs
      ord -> ord

data Command
  = Bootstrap
    { configPath :: FilePath
    , workstation :: Text
    }
  deriving (Show)

data WSState =
  WSState
  { props :: Set (Property WS)
  }

type WSStack a = ReaderT Settings (StateT WSState IO) a

newtype WS a
  = WS
  { unWS :: WSStack a
  } deriving newtype
  ( Monad
  , MonadReader Settings
  , MonadState WSState
  , Applicative
  , Functor
  , MonadIO
  )

data Settings = Settings
  { opts :: Options
  , configuration :: Configuration
  }

data Options = Options
  { command :: Command
  }
  deriving (Show)

