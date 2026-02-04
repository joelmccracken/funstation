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
-- import Shelly qualified as S
import Data.Text (Text)
import Data.Text qualified as T
-- import Path qualified
-- import Path ((</>))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
import Data.Maybe (isJust)
import GHC.Stack
import Data.List (intercalate)
import Shh (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure)
import Data.Yaml (decodeFileThrow)
-- import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Bool (bool)
import Control.Monad (void, forM_, forM, unless, when)
import Control.Monad.IO.Class
--import WSHS.Types
-- import WSHS.Types.Configuration
import Control.Monad.State
import Control.Monad.Reader
import Data.Time.Clock.POSIX (getPOSIXTime)
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
  | Dotfiles DotfilesP
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

cmd :: MonadIO m => Proc a -> m (Either Failure a)
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

-- | Expand a path using bash filename expansion (resolves ~, $HOME, etc.)
expandPath :: MonadIO m => Text -> m Text
expandPath path = do
  result <- cmd (exe "bash" "-c" ("echo " <> T.unpack path) |> captureTrim)
  pure $ either (const path) (TL.toStrict . TL.decodeUtf8) result

mvToBackup :: Text -> WS ()
mvToBackup path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- cmd (exe "mv" (T.unpack path) (T.unpack backupPath))
  case result of
    Right _ -> putStrLn' $ "Moved " <> path <> " to " <> backupPath
    Left err -> error $ "Failed to move file: " <> show err

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


data DotfilesP = DotfilesP
  { srcDir :: Text
  , destDir :: Maybe Text  -- ^ Destination base directory, defaults to "~/" if Nothing
  , files :: [DotfileConfig]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Get the destination base directory, defaulting to "~/" if not set
getDestDir :: DotfilesP -> Text
getDestDir p = maybe "~/" ensureTrailingSlash p.destDir
  where
    ensureTrailingSlash t = if T.isSuffixOf "/" t then t else t <> "/"

-- | Compute the full destination path for a dotfile config.
-- If dest is set and is an absolute path (starts with /), use it directly.
-- If dest is set but relative, prepend baseDestDir (dot prefix is ignored).
-- If dest is not set, derive from src with optional dot prefix.
computeDestPath :: Text -> DotfileConfig -> Text
computeDestPath baseDestDir f =
  case f.dest of
    Just d | T.isPrefixOf "/" d -> d
    Just d -> baseDestDir <> d  -- dot prefix ignored when dest is explicit
    Nothing -> baseDestDir <> (bool "" "." f.dot) <> f.src

-- | Check if a single dotfile is in the correct state
checkSingleDotfile :: MonadIO m => DotfileConfig -> Text -> Text -> m Bool
checkSingleDotfile f src dest = do
  case f.sort of
    Symlink -> do
      -- Check if dest is a symlink pointing to src
      target <- cmd (exe "readlink" (T.unpack dest) |> captureTrim)
      pure $ either (const False) (\t -> TL.toStrict (TL.decodeUtf8 t) == src) target
    Copy -> do
      -- Ensure dest is not a symlink, and contents match
      isSymlink <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -L ", T.unpack dest])
      if isSymlink
        then pure False  -- Wrong type: should be regular file, not symlink
        else do
          diffResult <- cmd (exe "diff" "-rq" (T.unpack src) (T.unpack dest) &> devNull)
          pure $ isRight diffResult

instance Prop DotfilesP where
  desc _ = "dotfiles management"
  attrs _ = mempty
  checker p = do
    let baseDestDir = getDestDir p
    results <- forM p.files $ \f -> do
      src <- expandPath $ p.srcDir <> "/" <> f.src
      dest <- expandPath $ computeDestPath baseDestDir f

      -- Verify source exists
      srcExists <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -e ", T.unpack src])
      unless srcExists $
        error $ "Source file does not exist: " <> T.unpack src
      result <- checkSingleDotfile f src dest
      pure result
    return $ all (== True) results
  fixer p = do
    let baseDestDir = getDestDir p
    void $ forM p.files $ \f -> do
      src <- expandPath $ p.srcDir <> "/" <> f.src
      dest <- expandPath $ computeDestPath baseDestDir f

      -- Verify source exists
      srcExists <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -e ", T.unpack src])
      unless srcExists $
        error $ "Source file does not exist: " <> T.unpack src

      -- Check for broken symlink (test -L succeeds but test -e fails)
      isBrokenSymlink <- do
        isLink <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -L ", T.unpack dest])
        exists <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -e ", T.unpack dest])
        pure (isLink && not exists)

      when isBrokenSymlink $ do
        putStrLn' $ "Removing broken symlink: " <> dest
        void $ cmd (exe "rm" (T.unpack dest))

      -- Check if dest exists (after removing broken symlink)
      destExists <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -e ", T.unpack dest])

      when destExists $ do
        -- Check if it's correct; if not, back it up
        isCorrect <- checkSingleDotfile f src dest
        unless isCorrect $ do
          putStrLn' $ "Backing up existing file: " <> dest
          mvToBackup dest

      -- Create symlink or copy
      case f.sort of
        Symlink -> do
          putStrLn' $ "Creating symlink: " <> dest <> " -> " <> src
          void $ cmd (exe "ln" "-s" (T.unpack src) (T.unpack dest))
        Copy -> do
          putStrLn' $ "Copying: " <> src <> " -> " <> dest
          void $ cmd (exe "cp" "-r" (T.unpack src) (T.unpack dest))
    pure ()

  dependencies _ = return []

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

data WSConfigDirP = WSConfigDirP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop WSConfigDirP where
  desc _ = "wshs configuration directory"
  attrs _ = mempty
  checker _ = do
    cfg <- asks configuration
    isRight <$> cmd (exe "bash" "-c" $ concat ["test -d ", T.unpack $ cfg.configDir])
  fixer _ = do
    cfg <- asks configuration
    let cloneCmd =
          intercalate " "
            [ "git clone"
            , "--branch"
            , T.unpack cfg.configRepoBranch
            , T.unpack cfg.configRepoUrl
            , T.unpack cfg.configDir
            ]
    result <- cmd (exe "bash" "-c" $ cloneCmd)
    case result of
      Right _ -> putStrLn' $ "Cloned repository to " <> cfg.configDir
      Left err -> putStrLn' $ "Failed to clone repository: " <> tshow err
  dependencies _ = return [IsProp HasGitP]

getProp :: Property -> IsProp
getProp (GitHomeDir p) = IsProp p
getProp (Dotfiles p) = IsProp p

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
    seen = wsstate.props
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
        ensureProperty (IsProp WSConfigDirP)
        forM_ (getProp <$> cfg.properties) ensureProperty
