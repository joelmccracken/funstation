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
import Data.Either (isRight)
import Shh (exe, tryFailure, devNull, (&>), capture , (|>), Proc, Failure)
import GHC.Stack
import Data.Yaml (decodeFileThrow, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Control.Monad (-- foldM,
                      void, forM_)
import Control.Monad.IO.Class

import Control.Monad.State
import Control.Monad.Reader


data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  }
  deriving (Generic, Show, FromJSON)





data OS = MacOS | Debian | Unknown
  deriving (Show, Eq)

data Property = Property
  { name :: Text
  , attrs :: Map.Map Text Text
  , checker :: WS Bool
  , fixer :: WS ()
  , dependencies :: WS [Property]
  }

instance Eq Property where
  a == b = a.name == b.name && a.attrs == b.attrs

instance Ord Property where
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
  { props :: Set Property
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

data Settings =
  Settings
  { opts :: Options
  , configuration :: Configuration
  }

data Options = Options
  { command :: Command
  }
  deriving (Show)

-- have a config.yaml in a known location
-- have a current.yaml at a known location for the "right now" settings
-- current workstation name, etc

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

detectOS :: WS OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> capture)
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

cmd :: Proc a -> WS (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c

xcodeCliTools :: Property
xcodeCliTools = Property
  { name = "Xcode CLI Tools"
  , attrs = mempty
  , checker = do
      result <- cmd (exe "pkgutil" "--pkg-info=com.apple.pkg.CLTools_Executables" &> devNull)
      return $ case result of
        Right _ -> True
        Left _  -> False
  , fixer = do
      result <- cmd (exe "sudo" "bash" "-c" "(xcodebuild -license accept; xcode-select --install) || exit 0"  &> devNull)
      case result of
        Right _ -> putStrLn' "Xcode CLI tools installed successfully"
        Left err -> putStrLn' $ "Failed to install Xcode CLI tools: " <> tshow err
  , dependencies = return []
  }

homebrew :: Property
homebrew = Property
  { name = "homebrew"
  , attrs = mempty
  , checker = do
      result <- cmd (exe "which" "brew" &> devNull)
      return $ case result of
        Right _ -> True
        Left _  -> False
  , fixer = do
      result <- cmd (exe "sudo" "bash" "-c" "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"  &> devNull)
      case result of
        Right _ -> putStrLn' "Homebrew installed successfully"
        Left err -> putStrLn' $ "Failed to install homebrew: " <> tshow err
  , dependencies = return [xcodeCliTools]
  }

gitMacOS :: Property
gitMacOS = Property
  { name = "git (macOS via Homebrew)"
  , attrs = mempty
  , checker = do
      result <- cmd (exe "which" "git" &> devNull)
      return $ case result of
        Right _ -> True
        Left _  -> False
  , fixer = do
      result <- cmd (exe "brew" "install" "git" &> devNull)
      case result of
        Right _ -> putStrLn' "Git installed successfully"
        Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  , dependencies = return [homebrew]
  }


aptUpdate :: Property
aptUpdate = Property
  { name = "apt package lists updated"
  , checker = return False  -- Always run update to be safe
  , fixer = do
      result <- cmd (exe "sudo" "apt-get" "update" &> devNull)
      case result of
        Right _ -> putStrLn' "apt package sets updated successfully"
        Left err -> putStrLn' $ "Failed to update apt: " <> tshow err
  , dependencies = return []
    , attrs = mempty

  }




gitDebian :: Property
gitDebian = Property
  { name = "git (Debian via apt)"
  , checker = do
      result <- cmd (exe "which" "git" &> devNull)
      return $ case result of
        Right _ -> True
        Left _  -> False
  , fixer = do
      result <- cmd(exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
      case result of
        Right _ -> putStrLn' "Git installed successfully"
        Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  , dependencies = return []
   , attrs = mempty

  }

basicSetup :: Property
basicSetup = Property
  { name = "basic setup"
  , attrs = mempty
  , checker = return True  -- This is a meta-property, always "valid" since it just orchestrates others
  , fixer = return ()       -- No direct action needed
  , dependencies = do
      os <- detectOS
      case os of
        MacOS -> return [homebrew, gitMacOS]
        Debian -> return [gitDebian]
        Unknown -> do
          putStrLn' "Warning: Unknown OS, skipping OS-specific setup"
          return []
  }

hasCmd :: Text -> Property
hasCmd name = do
  Property
    { name = "has cmd " <> name
    , attrs = mempty
    , dependencies = return []
    , checker = isRight <$> (cmd (exe "which" (T.unpack name)  &> devNull))
    , fixer = error $ "hasCmdP: unable to install command with this property " ++ T.unpack name
    }

hasGit :: Property
hasGit =
    (hasCmd "git") { fixer , dependencies }
  where
    dependencies = do
      os <- detectOS
      case os of
        MacOS -> return [homebrew]
        Debian -> return [aptUpdate]
        Unknown -> error "error: Unknown OS, unable to install git"
    fixer  = do
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


-- hasGit :: Property
-- hasGit =
--   Property
--   { name = "git is installed"
--   , checker = hasCmdP "git"
--   , dependencies = do
--       os <- detectOS
--       case os of
--         MacOS -> return [homebrew, gitMacOS]
--         Debian -> return [aptUpdate, gitDebian]
--         Unknown -> do
--           putStrLn' "Warning: Unknown OS, skipping OS-specific setup"
--           return []
--   }

gitTrackHome :: Property
gitTrackHome = Property
  { name = "git track home dir"
  , dependencies = return [hasGit]
  , attrs = mempty
  , checker = do
      result <- cmd (exe "which" "git" &> devNull)
      return $ case result of
        Right _ -> True
        Left _  -> False
  , fixer = do
      result <- cmd (exe "brew" "install" "git" &> devNull)
      case result of
        Right _ -> putStrLn' "Git installed successfully"
        Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  }

ensureProperty :: Property -> WS ()
ensureProperty prop = do
  wsstate <-  get
  let seen = wsstate.props

  if Set.member prop seen
    then return ()
    else do
      -- First, ensure all dependencies
      deps <- prop.dependencies
      -- TODO handle circular dependencies possibility
      forM_ deps ensureProperty

      -- Now check and fix this property
      putStrLn' $ "Checking property: " <> prop.name
      isValid <- prop.checker
      if isValid
        then putStrLn' "  ✓ Already valid"
        else do
          putStrLn' "  ✗ Invalid, applying fix..."
          prop.fixer

      put $ wsstate { props = Set.insert prop seen}

putStrLn' :: Text -> WS ()
putStrLn' t = liftIO $ putStrLn $ T.unpack t

main :: IO ()
main = do
  opts <- parseOptions
  case opts.command of
    Bootstrap cfgPath ws -> do
      cfg <- decodeFileThrow cfgPath :: IO Configuration
      void $ flip runStateT (WSState {props = mempty }) $ flip runReaderT (Settings opts cfg) $ unWS $ do
        liftIO $ print cfg
        putStrLn' $ "Workstation: " <> ws
        putStrLn' "\nEnsuring properties..."
        ensureProperty basicSetup

tshow :: Show s => s -> Text
tshow = T.pack . show
