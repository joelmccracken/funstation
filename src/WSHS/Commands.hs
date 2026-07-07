{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}

module WSHS.Commands where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
import Data.Maybe (isJust)
import System.FilePath (takeDirectory)
import Control.Monad (void, unless)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, asks)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shh (exe, devNull, (&>), captureTrim, (|>))
import Control.Monad.Except (MonadError, throwError)
import WSHS.Types
import WSHS.Sudo
import WSHS.Proc

detectOS :: (MonadIO m, MonadError WSError m) => m OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> captureTrim)
  case osCheck of
    Left e -> throwError $ WSFailure $ "error detecting OS: " <> tshow e
    Right "Darwin" -> return MacOS
    Right "Linux" -> detectLinuxOS
    Right _ -> return Unknown

detectLinuxOS :: MonadIO m => m OS
detectLinuxOS = do
  -- Check if it's Debian-based
  debianCheck <- cmd (exe "test" "-f" "/etc/debian_version" &> devNull)
  case debianCheck of
    Right _ -> return Debian
    Left _ -> return Unknown

which :: MonadIO m => Text -> m (Maybe Text)
which cmdName = do
  result <- cmd (exe "which" (T.unpack cmdName) |> captureTrim)
  pure $ either (const Nothing) (Just . TL.toStrict . TL.decodeUtf8) result

hasCmd' :: MonadIO m => Text -> m Bool
hasCmd' cmdName = isJust <$> which cmdName

-- | Check if a directory exists.
dirExists :: MonadIO m => Text -> m Bool
dirExists path = isRight <$> cmd (exe "test" "-d" (T.unpack path))

-- | Check if a file (or any path) exists.
fileExists :: MonadIO m => Text -> m Bool
fileExists path = isRight <$> cmd (exe "test" "-e" (T.unpack path))

-- | Create a directory (and any missing parents).
mkDir :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
mkDir path = do
  args' <- mkWSCmd ["mkdir", "-p", path]
  void $ cmd $ exe $ T.encodeUtf8 <$> args'

-- | Ensure a parent directory exists, using sudo only if needed.
ensureParentDir :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
ensureParentDir path = do
  let parentDir = T.pack $ takeDirectory (T.unpack path)
  exists <- dirExists parentDir
  unless exists $ do
    result <- privCmd WriteAccess parentDir ["mkdir", "-p", parentDir]
    case result of
      Right _ -> pure ()
      Left err -> throwError $ WSFailure $ "Failed to create directory " <> parentDir <> ": " <> tshow err

-- | Get "owner:group" for a path, for use with chown.
-- Uses ls -ld which works on both macOS and Linux.
getOwnerGroup :: MonadIO m => Text -> m Text
getOwnerGroup path = do
  result <- cmd (exe "ls" "-ld" (T.unpack path) |> captureTrim)
  case result of
    Left _ -> pure "root:root"
    Right bytes ->
      case words $ TL.unpack $ TL.decodeUtf8 bytes of
        (_:_:owner:group:_) -> pure $ T.pack owner <> ":" <> T.pack group
        _ -> pure "root:root"

-- | Move a file to a timestamped backup, using sudo only if needed.
mvToBackupAuto :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m Text
mvToBackupAuto path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- privCmd WriteAccess path ["mv", path, backupPath]
  case result of
    Right _ -> do
      putStrLn' $ "  Backed up " <> path <> " to " <> backupPath
      pure backupPath
    Left err -> throwError $ WSFailure $ "Failed to backup file: " <> tshow err

{-# DEPRECATED mvToBackupSudo "Use mvToBackupAuto instead" #-}
mvToBackupSudo :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m Text
mvToBackupSudo = mvToBackupAuto

-- | Check if a file has the desired contents.
-- Returns True if the file exists and matches, False otherwise.
fileContentsCheck :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> Text -> m Bool
fileContentsCheck path content = do
  -- Create temp file with desired content
  tempFileResult <- cmd (exe "mktemp" |> captureTrim)
  case tempFileResult of
    Left err -> throwError $ WSFailure $ "Failed to create temp file: " <> tshow err
    Right tempFileBytes -> do
      let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

      -- Write desired content to temp file
      liftIO $ TIO.writeFile tempFile content

      -- Check if target file exists
      targetExists <- fileExists path

      result <- if not targetExists
        then pure False  -- Target doesn't exist, needs fixing
        else do
          -- Compare using diff (only need read access to target file)
          sc <- asks (.sudoCmd)
          diffCmd <- liftIO $ mkPrivCmd sc ReadAccess path ["diff", "-q", T.pack tempFile, path]
          diffResult <- cmd $ exe (T.encodeUtf8 <$> diffCmd)  &> devNull
          pure $ isRight diffResult

      -- Clean up temp file
      -- TODO bracket to clean up
      void $ cmd $ exe ["rm", "-f", tempFile]

      pure result

-- | Ensure a file has the desired contents.
-- Returns Nothing if no change was needed, Just backupPath if the file was updated.
-- The backupPath will be empty string if no backup was needed (file didn't exist).
-- TODO think about what parts to reuse/share (e.g. with dotfiles code)
fileContentsFix :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> Text -> m (Maybe Text)
fileContentsFix path content = do
  -- First check if file already has correct contents
  isCorrect <- fileContentsCheck path content
  if isCorrect
    then pure Nothing  -- No change needed
    else do
      -- Create temp file with desired content
      tempFileResult <- cmd (exe "mktemp" |> captureTrim)
      case tempFileResult of
        Left err -> throwError $ WSFailure $ "Failed to create temp file: " <> tshow err
        Right tempFileBytes -> do
          let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

          -- Write desired content to temp file
          liftIO $ TIO.writeFile tempFile content

          -- Check if target exists; capture owner info before any changes
          targetExists <- fileExists path
          ownerGroup <- if targetExists
            then getOwnerGroup path
            else getOwnerGroup (T.pack $ takeDirectory (T.unpack path))

          -- Back up existing file if present
          backupPath <- if targetExists
            then mvToBackupAuto path
            else pure ""

          -- Move temp file to target location (only use sudo if needed)
          moveResult <- privCmd WriteAccess path ["mv", T.pack tempFile, path]
          case moveResult of
            Left err -> throwError $ WSFailure $ "Failed to move file to " <> path <> ": " <> tshow err
            Right _ -> pure ()

          -- Restore original ownership when sudo was used
          void $ privCmd WriteAccess path ["chown", ownerGroup, path]
          pure $ Just backupPath

-- Primitive WS utilities

tshow :: Show s => s -> Text
tshow = T.pack . show

putStrLn' :: MonadIO m => Text -> m ()
putStrLn' t = liftIO $ putStrLn $ T.unpack t

-- | Expand a path using bash filename expansion (resolves ~, $HOME, etc.)
expandPath :: MonadIO m => Text -> m Text
expandPath path = do
  result <- cmd $ (exe ["bash", "-c", ("echo " <> T.unpack path)] |> captureTrim)
  pure $ either (const path) (TL.toStrict . TL.decodeUtf8) result

mvToBackup :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
mvToBackup path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  args' <- mkWSCmd ["mv", path, backupPath]
  result <- cmd $ exe $ T.encodeUtf8 <$> args'
  case result of
    Right _ -> putStrLn' $ "Moved " <> path <> " to " <> backupPath
    Left err -> throwError $ WSFailure $ "Failed to move file: " <> tshow err

-- | Restart the Nix daemon (OS-aware)
restartNixDaemon :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => m ()
restartNixDaemon = do
  os <- detectOS
  case os of
    MacOS -> do
      liftIO $ putStrLn "Restarting Nix daemon (macOS)..."
      unloadArgs <- mkWSCmd ["sudo", "launchctl", "unload", "/Library/LaunchDaemons/org.nixos.nix-daemon.plist"]
      void $ cmd $ exe $ T.encodeUtf8 <$> unloadArgs
      loadArgs <- mkWSCmd ["sudo", "launchctl", "load", "/Library/LaunchDaemons/org.nixos.nix-daemon.plist"]
      void $ cmd $ exe $ T.encodeUtf8 <$> loadArgs
    Debian -> do
      liftIO $ putStrLn "Restarting Nix daemon (Debian)..."
      restartArgs <- mkWSCmd ["sudo", "systemctl", "restart", "nix-daemon.service"]
      void $ cmd $ exe $ T.encodeUtf8 <$> restartArgs
    Unknown -> throwError $ WSFailure "Cannot restart nix daemon: unknown OS"
  liftIO $ threadDelay 5000000
