{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module WSHS.Properties.Nix where

import WSHS.Types
import WSHS.Commands
import Shh (exe)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Data.Either (isRight)
import Control.Monad (unless, when)
import qualified Data.Map.Strict as Map

instance Prop NixDaemonP where
  desc _ = "Nix package manager (daemon mode)"
  attrs p = Map.fromList
    [ ("version", fromMaybe defaultNixVersion p.version)
    , ("interactive", tshow p.interactive)
    ]
  checker p = do
    nixInstalled <- hasCmd' "nix"
    if not nixInstalled
      then return False
      else case p.nixConf of
        Nothing -> return True  -- No config specified, nix installed = good
        Just desiredConf -> do
          let nixConfPath = "/etc/nix/nix.conf"
          fileContentsCheck nixConfPath desiredConf
  fixer p = do
    -- Check if Nix is already installed
    nixInstalled <- hasCmd' "nix"

    if nixInstalled
      then putStrLn' "Nix already installed, checking configuration..."
      else installNix

    -- Manage nix.conf if specified (runs whether or not we just installed)
    updateNixConf

    putStrLn' "Nix daemon setup complete."

    where
      installNix :: WS ()
      installNix = do
        let ver = fromMaybe defaultNixVersion p.version
        let installerUrl = "https://releases.nixos.org/nix/nix-" <> ver <> "/install"
        let yesFlag = if p.interactive then "" else " --yes"

        putStrLn' $ "Installing Nix " <> ver <> "..."

        let installCmd = "curl -L " <> T.unpack installerUrl <> " | sh -s -- --daemon" <> T.unpack yesFlag
        result <- cmd (exe "bash" "-c" installCmd)
        case result of
          Left err -> error $ "Nix installation failed: " <> show err
          Right _ -> pure ()

        -- Verify profile exists
        profileExists <- isRight <$> cmd (exe "test" "-e" nixDaemonProfile)
        unless profileExists $
          error $ "Nix installed, but cannot find profile file: " <> nixDaemonProfile

        putStrLn' ""
        putStrLn' "Nix installed. Add the following to your shell profile:"
        putStrLn' $ "  . " <> T.pack nixDaemonProfile
        putStrLn' ""

        restartNixDaemon

      updateNixConf :: WS ()
      updateNixConf = case p.nixConf of
        Nothing -> pure ()
        Just desiredConf -> do
          let nixConfPath = "/etc/nix/nix.conf"
          result <- fileContentsFix nixConfPath desiredConf
          case result of
            Nothing -> putStrLn' "  nix.conf already has correct contents"
            Just backupPath -> do
              putStrLn' $ "  Updated " <> nixConfPath
              when (backupPath /= "") $
                putStrLn' $ "  (backed up original to " <> backupPath <> ")"
              -- Restart daemon since config changed
              restartNixDaemon

  dependencies _ = return []
