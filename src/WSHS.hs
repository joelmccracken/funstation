{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WSHS (module WSHS, module WSHS.Types, module WSHS.Configuration, module WSHS.Properties.Dotfiles, module WSHS.Sudo, module WSHS.Commands) where

import WSHS.Sudo
import WSHS.Types
import WSHS.Commands
import WSHS.Configuration
import WSHS.Properties.Dotfiles
import WSHS.Properties.Git ()
import WSHS.Properties.MacOS ()
import WSHS.Properties.Debian ()
import WSHS.Properties.Basic
import WSHS.Properties.Nix ()
import WSHS.Properties.HomeManager ()
import WSHS.Properties.BitwardenSecrets ()

import Options.Applicative
import Options.Applicative qualified as App
import Data.Text qualified as T
import Data.Yaml (decodeFileThrow)
import qualified Data.Set as Set
import Control.Monad (forM_)
import Control.Concurrent (killThread)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except (MonadError, runExceptT)
import Data.Set (Set)
import System.Exit (exitFailure, exitSuccess)

getProp :: Property -> IsProp
getProp (GitHomeDir p) = IsProp p
getProp (GitHomeDirClone p) = IsProp p
getProp (Dotfiles p) = IsProp p
getProp (NixDaemon p) = IsProp p
getProp (HomeManager p) = IsProp p
getProp (HomebrewBundle p) = IsProp p
getProp (BitwardenSecrets p) = IsProp p

bootstrapParser :: Parser Command
bootstrapParser = Bootstrap
  <$> strArgument
      ( metavar "CONFIG"
     <> help "Path to the configuration YAML file" )
  <*> strArgument
      ( metavar "WORKSTATION"
     <> help "Name of the current workstation" )

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
    ( info (bootstrapParser <**> helper)
      ( progDesc "Bootstrap a new workstation" )
    )
 <> App.command "nix"
    ( info (Nix <$> nixSubcommandParser <**> helper)
      ( progDesc "Nix package manager utilities" )
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

parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "WSHS - Workstation Setup Helper System"
      <> header "wshs - manage workstation configurations"
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
    Bootstrap cfgPath ws -> doBootstrap opts cfgPath ws
    Nix NixRestart -> doNixRestart opts

  -- Clean up sudo refresh thread
  maybe (pure ()) killThread  sudoThread
 where
  doBootstrap opts cfgPath ws = do
    cfg <- decodeFileThrow cfgPath :: IO Configuration
    let
      bootstrapAct = do
        putStrLn' $ "Workstation: " <> ws
        putStrLn' "\nEnsuring properties..."
        ensureProperty (IsProp BasicSetupP)
        ensureProperty (IsProp $ configDirProp cfg)
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

  configDirProp cfg =
    WSConfigDirP
    { configDir = cfg.configDir
    , configRepoUrl = cfg.configRepoUrl
    , configRepoBranch = cfg.configRepoBranch
    }

  doNixRestart opts = do
    result <-
      runExceptT (runReaderT restartNixDaemon
        (Settings { opts = opts, sudoCmd = "sudo" }))
    failLeft result

failLeft :: MonadIO m => Either WSError a -> m a
failLeft = either (liftIO . handleFail) pure
 where
  handleFail (WSFailure msg) = do
    putStrLn $ "wshs: error: " <> T.unpack msg
    exitFailure
  handleFail WSAborted = do
    putStrLn "Run aborted."
    exitSuccess
