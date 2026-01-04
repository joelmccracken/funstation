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


module WSHS (module WSHS) where

import Options.Applicative
import Options.Applicative qualified as App
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
import Data.Maybe (isJust)
import GHC.Stack
import Shh (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure )
import Data.Yaml (decodeFileThrow)
-- import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Bool (bool)
import Control.Monad (void, forM_)
import Control.Monad.IO.Class
--import WSHS.Types
-- import WSHS.Types.Configuration
import Control.Monad.State
import Control.Monad.Reader
-- import WSHS.Commands qualified as Cmd
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Aeson.Types hiding (Parser, Options)
-- import Data.Aeson.TH hiding (Options)
import GHC.Generics (Generic)

-- have a config.yaml in a known location
-- have a current.yaml at a known location for the "right now" settings
-- current workstation name, etc

data WSState =
  WSState
  { props :: Set IsProp
  }

type WSStack a = ReaderT Settings (StateT WSState IO) a

data Settings = Settings
  { opts :: Options
  , configuration :: Configuration
  }

data Property
  = GitHomeDir GitTrackHomeDirP
  | BasicSetup BasicSetupP
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
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  , properties :: [Property]
  }
  deriving (Generic, Show, FromJSON)

data Command
  = Bootstrap
    { configPath :: FilePath
    , workstation :: Text
    }
  deriving (Show)

data Options = Options
  { command :: Command
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

data OS = MacOS | Debian | Unknown
  deriving (Show, Eq)

putStrLn' :: Text -> WS ()
putStrLn' t = liftIO $ putStrLn $ T.unpack t

tshow :: Show s => s -> Text
tshow = T.pack . show

cmd :: Proc a -> WS (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c

detectOS :: WS OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> captureTrim)
  case osCheck of
    Left e -> error $ "error detecting OS: " <> show e
    Right "Darwin" -> return MacOS
    Right "Linux" -> detectLinuxOS
    Right _ -> return Unknown

detectLinuxOS :: WS  OS
detectLinuxOS = do
  -- Check if it's Debian-based
  debianCheck <- cmd (exe "test" "-f" "/etc/debian_version" &> devNull)
  case debianCheck of
    Right _ -> return Debian
    Left _ -> return Unknown

which :: Text -> WS (Maybe Text)
which cmdName = do
  result <- cmd (exe "which" (T.unpack cmdName) |> captureTrim)
  pure $ either (const Nothing) (Just . TL.toStrict . TL.decodeUtf8 ) result

hasCmd' :: Text -> WS Bool
hasCmd' cmdName = isJust <$> which cmdName

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

-- unIsProp :: IsProp -> (forall p. IsProp p => p -> a) -> a
-- unIsProp (IsProp p) f = f p

-- data ExampleP = ExampleP
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- instance Prop ExampleP where
--   desc _ = undefined
--   attrs _ = undefined
--   checker _ = undefined
--   fixer _ = undefined
--   dependencies _ = undefined


data GitTrackHomeDirP = GitTrackHomeDirP { gitDir :: Text }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitTrackHomeDirP where
  desc _ = "git track home dir"
  attrs p = Map.fromList [("gitDir", gitDir p)]
  checker p =
    isRight <$> cmd (exe "bash" "-c" $ concat ["test -d $HOME/", T.unpack $ gitDir p])
  fixer p = do
    let cmdtxt = [ "export GIT_DIR='"
                 , T.unpack $ gitDir p
                 , "'; "
                 , concat [ "("
                          , "cd $HOME; "
                          , "git init .; "
                          , "git config --local --get-all core.bare true >/dev/null && "
                          , "git config --local --replace-all core.bare false true "
                          , ")"
                          ]
                 ]
    result <- cmd (exe "bash" "-c" $ concat cmdtxt )
    case result of
      Right _ -> putStrLn' "home dir git tracking setup successfully"
      Left err -> putStrLn' $ "Failed setting up home dir git tracking: " <> tshow err

  dependencies _ = return [IsProp HasGitP]

data BasicSetupP = BasicSetupP
 deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop BasicSetupP where
  desc _ = "basic setup"
  attrs _ = mempty
  checker _ = return True -- dummy prop, wrapper for dependencies
  fixer _ = return () -- all action in dependencies
  dependencies _ = return [ (IsProp HasGitP) ]

data GitMacOSP = GitMacOSP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitMacOSP where
  desc _ = "git (macOS via Homebrew)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd (exe "brew" "install" "git" &> devNull)
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = return [(IsProp HomebrewP)]


data GitDebianP = GitDebianP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitDebianP where
  desc _ = "git (Debian via apt)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd(exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = do
    hasCmd' "git" >>= bool (return [(IsProp AptUpdateP)]) (return [])

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop HasGitP where
  desc _ = "has command `git` installed"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  dependencies _ = do
    os <- detectOS
    case os of
      MacOS -> return [(IsProp HomebrewP)]
      Debian -> return [(IsProp AptUpdateP)]
      Unknown -> error "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- detectOS
    case os of
      Unknown -> error "error: Unknown OS, unable to install git"
      MacOS -> do
        result <- cmd (exe "brew" "install" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
      Debian -> do
        result <- cmd (exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err

data XCodeCLIToolsP = XCodeCLIToolsP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop XCodeCLIToolsP where
  desc _ = "Xcode CLI Tools"
  attrs _ = mempty
  checker _ = isRight <$> cmd (exe "pkgutil" "--pkg-info=com.apple.pkg.CLTools_Executables" &> devNull)
  fixer _ = do
      result <- cmd (exe "sudo" "bash" "-c" "(xcodebuild -license accept; xcode-select --install) || exit 0"  &> devNull)
      case result of
        Right _ -> putStrLn' "Xcode CLI tools installed successfully"
        Left err -> putStrLn' $ "Failed to install Xcode CLI tools: " <> tshow err
  dependencies _ = return []

data HomebrewP = HomebrewP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HomebrewP where
  desc _ = "homebrew package manager for macOS"
  attrs _ = mempty
  checker _ = hasCmd' "brew"
  fixer _ = do
    result <- cmd (exe "sudo" "bash" "-c" "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"  &> devNull)
    case result of
      Right _ -> putStrLn' "Homebrew installed successfully"
      Left err -> putStrLn' $ "Failed to install homebrew: " <> tshow err
  dependencies _ = return [ (IsProp XCodeCLIToolsP) ]

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop AptUpdateP where
  desc _ = "apt package lists updated"
  attrs _ = mempty
  checker _ = return False  -- Always run update to be safe
  fixer _ = do
   result <- cmd (exe "sudo" "apt-get" "update" &> devNull)
   case result of
     Right _ -> putStrLn' "apt package sets updated successfully"
     Left err -> putStrLn' $ "Failed to update apt: " <> tshow err
  dependencies _ = return []

getProp :: Property -> IsProp
getProp (GitHomeDir p) = IsProp p
getProp (BasicSetup p) = IsProp p

bootstrapParser :: Parser Command
bootstrapParser = Bootstrap
  <$> strArgument
      ( metavar "CONFIG"
     <> help "Path to the configuration YAML file" )
  <*> strArgument
      ( metavar "WORKSTATION"
     <> help "Name of the current workstation" )

commandParser :: Parser Command
commandParser = subparser
  ( App.command "bootstrap"
    ( info (bootstrapParser <**> helper)
      ( progDesc "Bootstrap a new workstation" )
    )
  )

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "WSHS - Workstation Setup Helper System"
      <> header "wshs - manage workstation configurations"
    )

ensureProperty :: Prop p => p -> WS ()
ensureProperty prop = do
  wsstate <- get
  let
    seen :: Set IsProp
    seen = wsstate.props --

  if Set.member (IsProp prop) seen
    then return ()
    else do
      -- First, ensure all dependencies
      -- here should print the reason a prop is beign executed
      --  ("A depends on B, checking A")
      deps <- dependencies prop
      -- TODO handle circular dependencies possibility
      forM_ deps $ ensureProperty

      -- Now check and fix this property
      putStrLn' $ "Checking property: " <> desc prop
      isValid <- checker prop
      if isValid
        then putStrLn' "  ✓ Already valid"
        else do
          putStrLn' "  ✗ Invalid, applying fix..."
          fixer prop

      put $ wsstate { props = Set.insert (IsProp prop) seen}

main :: IO ()
main = do
  opts <- parseOptions
  case opts.command of
    Bootstrap cfgPath ws -> do
      cfg <- decodeFileThrow cfgPath :: IO Configuration
      void $ flip runStateT (WSState { props = mempty }) $ flip runReaderT (Settings opts cfg) $ unWS $ do
        liftIO $ print cfg
        putStrLn' $ "Workstation: " <> ws
        putStrLn' "\nEnsuring properties..."
        ensureProperty (IsProp BasicSetupP)
        forM_ (getProp <$> cfg.properties) ensureProperty
