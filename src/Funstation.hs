{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Funstation (module Funstation, module Funstation.Types, module Funstation.Configuration, module Funstation.Properties.Dotfiles, module Funstation.Sudo, module Funstation.Commands) where

import Funstation.Sudo
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Funstation.Configuration
import Funstation.Properties.Dotfiles
import Funstation.Properties.HasGit ()
import Funstation.Properties.GitHomeDir (resolveGitDir, GitHomeDirP(..))
import Funstation.Properties.XCodeCLITools ()
import Funstation.Properties.Homebrew ()
import Funstation.Properties.HomebrewBundle ()
import Funstation.Properties.AptUpdate ()
import Funstation.Properties.CoreDependencies
import Funstation.Properties.NixDaemon ()
import Funstation.Properties.HomeManager ()
import Funstation.Properties.BitwardenSecrets ()

import Options.Applicative
import Options.Applicative qualified as App
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileThrow)
import qualified Data.Set as Set
import Control.Monad (forM_, void)
import Control.Concurrent (killThread)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except (MonadError, runExceptT)
import Data.Set (Set)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Exit (exitFailure, exitSuccess)

getProp :: Property -> IsProp
getProp (GitHomeDir p) = IsProp p
getProp (GitClone p) = IsProp p
getProp (Dotfiles p) = IsProp p
getProp (NixDaemon p) = IsProp p
getProp (HomeManager p) = IsProp p
getProp (HomebrewBundle p) = IsProp p
getProp (BitwardenSecrets p) = IsProp p

bootstrapParser :: Parser Command
bootstrapParser = pure Bootstrap

nixSubcommandParser :: Parser NixSubcommand
nixSubcommandParser = subparser
  ( App.command "restart"
    ( info (pure NixRestart <**> helper)
      ( progDesc "Restart the Nix daemon" )
    )
  )

commandParser :: Parser Command
commandParser = subparser
  ( App.command "bootstrap"
    ( info (pure Bootstrap <**> helper)
      ( progDesc "Bootstrap a new workstation" )
    )
 <> App.command "nix"
    ( info (Nix <$> nixSubcommandParser <**> helper)
      ( progDesc "Nix package manager utilities" )
    )
 <> App.command "status"
    ( info (pure Status <**> helper)
      ( progDesc "Show status of managed properties" )
    )
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> switch
      ( long "sudo-cache"
     <> help "Cache sudo credentials and refresh in background (prompts once at start)"
      )
  <*> optional (T.pack <$> strOption
      ( long "sudo-pass-file"
     <> metavar "FILE"
     <> help "File containing sudo password (first line only, implies --sudo-cache)"
      ))
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print each command before running it"
      )
  <*> switch
      ( long "interactive"
     <> short 'i'
     <> help "Prompt before each command; implies --verbose"
      )
  <*> strOption
      ( long "config"
     <> metavar "CONFIG_FILE"
     <> help "Path to the configuration YAML file"
      )
  <*> optional (T.pack <$> strOption
      ( long "workstation"
     <> metavar "WORKSTATION"
     <> help "Name of the current workstation. Usable by properties."
      ))

parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "funstation - a workstation configuration tool"
      <> header "fun - manage workstation configurations"
    )

-- | Path to the file where the resolved workstation name is persisted, so other
-- tools can read it. Lives in the state dir, matching the BitwardenSecrets convention.
workstationNameStateFile :: IO FilePath
workstationNameStateFile = do
  home <- getHomeDirectory
  pure $ home </> ".local" </> "state" </> "funstation" </> "workstation"

-- | Read the saved workstation name, if the state file exists and is non-empty.
readWorkstationNameFromState :: IO (Maybe Text)
readWorkstationNameFromState = do
  path <- workstationNameStateFile
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      contents <- T.strip <$> TIO.readFile path
      pure $ if T.null contents then Nothing else Just contents

-- | Persist the workstation name to the state file (creating parent dirs).
writeWorkstationState :: Text -> IO ()
writeWorkstationState name = do
  path <- workstationNameStateFile
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (name <> "\n")

-- | The set of valid workstation names: the names declared in the config, or the
-- single default @"workstation"@ when the config declares none.
knownWorkstations :: [Text] -> [Text]
knownWorkstations [] = ["workstation"]
knownWorkstations ns = ns

-- | The resolved workstation name, tagged with where it came from. The source
-- determines side effects: only a name that came from the CLI is written back to the
-- saved state.
data WorkstationSource
  = FromCLI Text        -- ^ supplied via @--workstation@ on the command line
  | FromStateFile Text  -- ^ read from the saved state file
  | FromDefault Text    -- ^ the sole known workstation (a single declared name, or the
                        --   built-in @"workstation"@ default)
  deriving (Show, Eq)

-- | The resolved workstation name, regardless of its source.
workstationSourceName :: WorkstationSource -> Text
workstationSourceName (FromCLI name)       = name
workstationSourceName (FromStateFile name) = name
workstationSourceName (FromDefault name)   = name

-- | Pure core of workstation resolution. Given the config's declared names, the raw
-- CLI @--workstation@ value, and the (non-empty) saved state, decide the current name
-- and where it came from. CLI wins; otherwise a saved name is used; otherwise the
-- single known name is used, erroring when several are declared. Any name coming from
-- CLI or state is validated against the known set.
resolveWorkstationName
  :: Configuration   -- ^ workstation names declared in the config (before defaulting)
  -> Maybe Text      -- ^ raw CLI @--workstation@ value
  -> Maybe Text      -- ^ saved state-file contents, if present and non-empty
  -> Either Text WorkstationSource  -- ^ @Left errorMessage@ or @Right source@
resolveWorkstationName cfg cliName savedName =
  case cliName of
    Just name -> FromCLI <$> validate "--workstation" name
    Nothing -> case savedName of
      Just name -> FromStateFile <$> validate "saved state" name
      Nothing -> case known of
        [single] -> Right (FromDefault single)
        _ -> Left "multiple workstations are defined; specify one with --workstation"
 where
  configNames = workstationName <$> cfg.workstations
  known = knownWorkstations configNames
  validate src name
    | name `elem` known = Right name
    | otherwise = Left $ "unknown workstation " <> name <> " (from " <> src
                      <> "); known workstations: " <> T.intercalate ", " known

-- | Resolve the current workstation name, reading and (when the name comes from the
-- CLI) persisting the state file. Errors abort with a clear message, consistent with
-- how config-decode failures surface.
resolveWorkstation :: Configuration -> Options -> IO Text
resolveWorkstation cfg opts = do
  saved <- readWorkstationNameFromState
  case resolveWorkstationName cfg opts.workstation saved of
    Left err -> do
      putStrLn $ "fun: error: " <> T.unpack err
      exitFailure
    Right source -> do
      case source of
        FromCLI name -> writeWorkstationState name
        FromStateFile _ -> pure ()
        FromDefault _ -> pure ()
      pure (workstationSourceName source)

ensureProperty
  :: ( Prop p
     , MonadIO m
     , MonadReader Settings m
     , MonadError WSError m
     , MonadState WSState m
     )
  => p -> m ()
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

  -- Resolve (and, when given on the CLI, persist) the current workstation name
  -- once, from the global config, and make it available to every command.
  cfg <- decodeFileThrow opts.configPath :: IO Configuration
  ws <- resolveWorkstation cfg opts

  -- Initialize sudo caching if requested
  sudoThread <- if opts.sudoCache
    then initSudoCache opts.sudoPassFile
    else pure Nothing

  case opts.command of
    Bootstrap -> doBootstrap opts ws cfg
    Nix NixRestart -> doNixRestart opts ws
    Status -> doStatus opts ws cfg

  -- Clean up sudo refresh thread
  maybe (pure ()) killThread  sudoThread
 where
  doBootstrap opts ws cfg = do
    let
      bootstrapAct = do
        putStrLn' $ "Workstation: " <> ws
        putStrLn' "\nEnsuring properties..."
        ensureProperty (IsProp CoreDependenciesP)
        forM_ (getProp <$> cfg.properties) ensureProperty
    result <-
      evalStateT
        (runExceptT
           (runReaderT
              bootstrapAct
              (settings opts ws)))
        wsState
    failLeft result

  wsState = WSState { props = mempty }
  settings opts ws = Settings { opts = opts, sudoCmd = "sudo", workstation = ws }

  doNixRestart opts ws = do
    result <-
      runExceptT (runReaderT restartNixDaemon (settings opts ws))
    failLeft result

  doStatus opts ws cfg = do
    let gitHomeDirs = [ p | GitHomeDir p <- cfg.properties ]
    case gitHomeDirs of
      [] -> putStrLn "Nothing to report."
      (p:_) -> do
        expandedHomeDir <- expandPath (fromMaybe "~" p.homeDir)
        expandedGitDir  <- resolveGitDir expandedHomeDir <$> expandPath p.gitDir
        let
          runGitStatus = do
            runCmd [ "bash", "-c" , T.intercalate " "
                                      [ "cd", expandedHomeDir, ";", "git", "--git-dir", expandedGitDir
                                      , "status" ]
                   ] id

        result <- runExceptT $ runReaderT runGitStatus (settings opts ws)
        void $ failLeft result

failLeft :: MonadIO m => Either WSError a -> m a
failLeft = either (liftIO . handleFail) pure
 where
  handleFail (WSFailure msg) = do
    putStrLn $ "fun: error: " <> T.unpack msg
    exitFailure
  handleFail WSAborted = do
    putStrLn "Run aborted."
    exitSuccess
