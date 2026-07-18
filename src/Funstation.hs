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
import Funstation.Properties.Nix ()
import Funstation.Properties.HomeManager ()
import Funstation.Properties.BitwardenSecrets ()

import Options.Applicative
import Options.Applicative qualified as App
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileThrow)
import qualified Data.Set as Set
import Control.Monad (forM_, void)
import Control.Concurrent (killThread)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except (MonadError, runExceptT)
import Data.Set (Set)
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

statusParser :: Parser Command
statusParser = Status
  <$> optional (strOption
      ( long "config"
     <> metavar "FILE"
     <> help "Path to the configuration YAML file (default: ~/.config/funstation/config.yaml)" ))

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
    ( info (statusParser <**> helper)
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
  <*> strOption
      ( long "workstation"
     <> metavar "WORKSTATION"
     <> help "Name of the current workstation. Usable by properties."
      )

parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "funstation - a workstation configuration tool"
      <> header "fun - manage workstation configurations"
    )

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

  -- Initialize sudo caching if requested
  sudoThread <- if opts.sudoCache
    then initSudoCache opts.sudoPassFile
    else pure Nothing

  case opts.command of
    Bootstrap -> doBootstrap opts opts.configPath opts.workstation
    Nix NixRestart -> doNixRestart opts
    Status mCfg -> doStatus opts mCfg

  -- Clean up sudo refresh thread
  maybe (pure ()) killThread  sudoThread
 where
  doBootstrap opts cfgPath ws = do
    cfg <- decodeFileThrow cfgPath :: IO Configuration
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
              (settings opts)))
        wsState
    failLeft result

  wsState = WSState { props = mempty }
  settings opts = Settings { opts = opts, sudoCmd = "sudo" }

  doNixRestart opts = do
    result <-
      runExceptT (runReaderT restartNixDaemon
        (Settings { opts = opts, sudoCmd = "sudo" }))
    failLeft result

  doStatus opts mCfg = do
    let cfgPath = fromMaybe "~/.config/funstation/config.yaml" (fmap T.pack mCfg)
    expandedCfgPath <- T.unpack <$> expandPath cfgPath
    cfg <- decodeFileThrow expandedCfgPath :: IO Configuration
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

        result <- runExceptT $ runReaderT runGitStatus
          (Settings { opts = opts, sudoCmd = "sudo" })
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
