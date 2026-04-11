{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module WSHS.Types where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except (ExceptT, MonadError)

-- Monad stack

data WSState =
  WSState
  { props :: Set IsProp
  }

data WSError
  = WSFailure Text
  | WSAborted
  deriving (Show)

type WSStack a = ReaderT Settings (ExceptT WSError (StateT WSState IO)) a

data Settings = Settings
  { opts :: Options
  , sudoCmd :: String  -- ^ Command to use for privilege escalation, e.g. "sudo" or "env" in tests
  }

data Options = Options
  { command :: Command
  , sudoCache :: Bool  -- ^ If True, cache sudo credentials and refresh in background
  , sudoPassFile :: Maybe Text  -- ^ Optional path to file containing sudo password
  , verbose :: Bool  -- ^ If True, print each command before running it
  , interactive :: Bool  -- ^ If True, prompt user before each command
  }
  deriving (Show)

newtype WS a
  = WS
  { unWS :: WSStack a
  }
  deriving newtype
  ( Monad
  , MonadReader Settings
  , MonadState WSState
  , Applicative
  , Functor
  , MonadIO
  , MonadError WSError
  )

-- Enum/command types

data OS = MacOS | Debian | Unknown
  deriving (Show, Eq)

data NixSubcommand
  = NixRestart
  deriving (Show)

data Command
  = Bootstrap
    { configPath :: FilePath
    , workstation :: Text
    }
  | Nix NixSubcommand
  | Status
    { configFile :: Maybe FilePath
    }
  deriving (Show)

-- Prop class and existential

class Prop p where
  desc :: p -> Text
  attrs :: p -> Map.Map Text Text
  -- checker: return true if property already fulfilled, false if fixer is required
  checker      :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => p -> m Bool
  fixer        :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => p -> m ()
  dependencies :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => p -> m [IsProp]

data IsProp where
  IsProp :: Prop p => p -> IsProp

instance Prop IsProp where
  desc (IsProp p) =  desc p
  attrs (IsProp p) =  attrs p
  checker (IsProp p) =  checker p
  fixer (IsProp p) =  fixer p
  dependencies (IsProp p) =  dependencies p

instance Eq IsProp where
  (IsProp a) == (IsProp b) = do
    desc a == desc b && attrs a == attrs b

instance Ord IsProp  where
  (IsProp a) `compare` (IsProp b) =
    case desc a `compare` desc b of
      EQ ->  attrs a `compare` attrs b
      ord -> ord
