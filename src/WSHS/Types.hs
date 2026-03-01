{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.Aeson.Types hiding (Parser, Options)
import GHC.Generics (Generic)

-- Monad stack

data WSState =
  WSState
  { props :: Set IsProp
  }

type WSStack a = ReaderT Settings (StateT WSState IO) a

data Settings = Settings
  { opts :: Options
  , configuration :: Configuration
  , sudoCmd :: String  -- ^ Command to use for privilege escalation, e.g. "sudo" or "env" in tests
  }

data Options = Options
  { command :: Command
  , sudoCache :: Bool  -- ^ If True, cache sudo credentials and refresh in background
  , sudoPassFile :: Maybe Text  -- ^ Optional path to file containing sudo password
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
  deriving (Show)

-- Config/property types

data Property
  = GitHomeDir GitTrackHomeDirP
  | Dotfiles DotfilesP
  | NixDaemon NixDaemonP
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
  { configDir :: Text
  , configRepoUrl :: Text
  , configRepoOrigin :: Text
  , configRepoBranch :: Text
  , properties :: [Property]
  }
  deriving (Generic, Show, FromJSON)

-- Prop class and existential

class Prop p where
  desc :: p -> Text
  attrs :: p -> Map.Map Text Text
  -- checker: return true if property already fulfilled, false if fixer is required
  checker :: p -> WS Bool
  fixer :: p -> WS ()
  dependencies :: p -> WS [IsProp]

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

-- Leaf property data types

data GitTrackHomeDirP = GitTrackHomeDirP { gitDir :: Text }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DotfileConfig = DotfileConfig
  { src :: Text
  , dest :: Maybe Text  -- ^ Optional destination path; if absolute, used directly
  , dot :: Bool
  , sort :: DotfileSort
  , dir :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DotfileSort
  = Symlink
  | Copy
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Describes the difference between desired and current filesystem state for a dotfile
data DotfileDiff
  = DotfileCorrect              -- ^ Already in the desired state, no action needed
  | DotfileMissing              -- ^ Destination doesn't exist, needs to be created
  | DotfileBrokenSymlink        -- ^ Destination is a broken symlink, needs removal and recreation
  | DotfileWrong                -- ^ Destination exists but has wrong content/type, needs backup and recreation
  | DotfileSrcMissing Text      -- ^ Error: source file doesn't exist (carries the missing path)
  deriving (Eq, Show)

data DotfilesP = DotfilesP
  { srcDir :: Text
  , destDir :: Maybe Text  -- ^ Destination base directory, defaults to "~/" if Nothing
  , files :: [DotfileConfig]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data BasicSetupP = BasicSetupP
 deriving (Eq, Show, Generic, FromJSON, ToJSON)

data GitMacOSP = GitMacOSP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data GitDebianP = GitDebianP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data XCodeCLIToolsP = XCodeCLIToolsP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HomebrewP = HomebrewP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data WSConfigDirP = WSConfigDirP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NixDaemonP = NixDaemonP
  { version :: Maybe Text      -- ^ Nix version to install, defaults to "2.24.14"
  , interactive :: Bool        -- ^ If True, allow user to answer installer prompts; if False, pass --yes
  , nixConf :: Maybe Text      -- ^ Desired contents of /etc/nix/nix.conf (optional)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Constants

defaultNixVersion :: Text
defaultNixVersion = "2.24.14"

nixDaemonProfile :: FilePath
nixDaemonProfile = "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
