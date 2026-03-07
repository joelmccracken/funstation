{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}

module WSHS.Commands where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
-- import Data.ByteString qualified as BS
-- import Data.ByteString.UTF8 qualified as BS
import Data.Maybe (isJust)
import System.FilePath (takeDirectory)
import Control.Monad (void, unless)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shh (exe, devNull, (&>), captureTrim, (|>), Failure, Proc, tryFailure)
import GHC.Stack (withFrozenCallStack)
import WSHS.Types
import WSHS.Sudo

detectOS :: MonadIO m => m OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> captureTrim)
  case osCheck of
    Left e -> error $ "error detecting OS: " <> show e
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

which :: Text -> WS (Maybe Text)
which cmdName = do
  result <- cmd (exe "which" (T.unpack cmdName) |> captureTrim)
  pure $ either (const Nothing) (Just . TL.toStrict . TL.decodeUtf8) result

hasCmd' :: Text -> WS Bool
hasCmd' cmdName = isJust <$> which cmdName

-- | Ensure a parent directory exists, using sudo only if needed.
ensureParentDir :: Text -> WS ()
ensureParentDir path = do
  let parentDir = T.pack $ takeDirectory (T.unpack path)
  exists <- isRight <$> cmd (exe "test" "-d" (T.unpack parentDir))
  unless exists $ do
    result <- privCmd WriteAccess parentDir ["mkdir", "-p", T.unpack parentDir]
    case result of
      Right _ -> pure ()
      Left err -> error $ "Failed to create directory " <> T.unpack parentDir <> ": " <> show err

-- | Get "owner:group" for a path, for use with chown.
-- Uses ls -ld which works on both macOS and Linux.
getOwnerGroup :: Text -> WS Text
getOwnerGroup path = do
  result <- cmd (exe "ls" "-ld" (T.unpack path) |> captureTrim)
  case result of
    Left _ -> pure "root:root"
    Right bytes ->
      case words $ TL.unpack $ TL.decodeUtf8 bytes of
        (_:_:owner:group:_) -> pure $ T.pack owner <> ":" <> T.pack group
        _ -> pure "root:root"

-- | Run a privilege-escalating command in the WS monad.
-- Reads the sudo command from Settings; uses AccessMode to choose the
-- permission check (read or write).
privCmd :: AccessMode -> Text -> [String] -> WS (Either Failure ())
privCmd mode pth args = do
  sc <- asks (.sudoCmd)
  c  <- liftIO $ mkPrivCmd sc mode pth args
  cmd c

-- | Move a file to a timestamped backup, using sudo only if needed.
mvToBackupAuto :: Text -> WS Text
mvToBackupAuto path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- privCmd WriteAccess path ["mv", T.unpack path, T.unpack backupPath]
  case result of
    Right _ -> do
      putStrLn' $ "  Backed up " <> path <> " to " <> backupPath
      pure backupPath
    Left err -> error $ "Failed to backup file: " <> show err

{-# DEPRECATED mvToBackupSudo "Use mvToBackupAuto instead" #-}
mvToBackupSudo :: Text -> WS Text
mvToBackupSudo = mvToBackupAuto

-- | Check if a file has the desired contents.
-- Returns True if the file exists and matches, False otherwise.
fileContentsCheck :: Text -> Text -> WS Bool
fileContentsCheck path content = do
  -- Create temp file with desired content
  tempFileResult <- cmd (exe "mktemp" |> captureTrim)
  case tempFileResult of
    Left err -> error $ "Failed to create temp file: " <> show err
    Right tempFileBytes -> do
      let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

      -- Write desired content to temp file
      liftIO $ TIO.writeFile tempFile content

      -- Check if target file exists
      targetExists <- isRight <$> cmd (exe "test" "-e" (T.unpack path))

      result <- if not targetExists
        then pure False  -- Target doesn't exist, needs fixing
        else do
          -- Compare using diff (only need read access to target file)
          sc <- asks (.sudoCmd)
          diffCmd <- liftIO $ mkPrivCmd sc ReadAccess path ["diff", "-q", tempFile, T.unpack path]
          diffResult <- cmd $ diffCmd &> devNull
          pure $ isRight diffResult

      -- Clean up temp file
      -- TODO bracket to clean up
      void $ cmd $ exe ["rm", "-f", tempFile]

      pure result

-- | Ensure a file has the desired contents.
-- Returns Nothing if no change was needed, Just backupPath if the file was updated.
-- The backupPath will be empty string if no backup was needed (file didn't exist).
-- TODO think about what parts to reuse/share (e.g. with dotfiles code)
fileContentsFix :: Text -> Text -> WS (Maybe Text)
fileContentsFix path content = do
  -- First check if file already has correct contents
  isCorrect <- fileContentsCheck path content
  if isCorrect
    then pure Nothing  -- No change needed
    else do
      -- Create temp file with desired content
      tempFileResult <- cmd (exe "mktemp" |> captureTrim)
      case tempFileResult of
        Left err -> error $ "Failed to create temp file: " <> show err
        Right tempFileBytes -> do
          let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

          -- Write desired content to temp file
          liftIO $ TIO.writeFile tempFile content

          -- Check if target exists; capture owner info before any changes
          targetExists <- isRight <$> cmd (exe "test" "-e" (T.unpack path))
          ownerGroup <- if targetExists
            then getOwnerGroup path
            else getOwnerGroup (T.pack $ takeDirectory (T.unpack path))

          -- Back up existing file if present
          backupPath <- if targetExists
            then mvToBackupAuto path
            else pure ""

          -- Move temp file to target location (only use sudo if needed)
          moveResult <- privCmd WriteAccess path ["mv", tempFile, T.unpack path]
          case moveResult of
            Left err -> error $ "Failed to move file to " <> T.unpack path <> ": " <> show err
            Right _ -> pure ()

          -- Restore original ownership when sudo was used
          void $ privCmd WriteAccess path ["chown", T.unpack ownerGroup, T.unpack path]
          pure $ Just backupPath

-- Primitive WS utilities

tshow :: Show s => s -> Text
tshow = T.pack . show

cmd :: MonadIO m => Proc a -> m (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c

putStrLn' :: Text -> WS ()
putStrLn' t = liftIO $ putStrLn $ T.unpack t

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

-- | Restart the Nix daemon (OS-aware)
restartNixDaemon :: MonadIO m => m ()
restartNixDaemon = do
  os <- detectOS
  case os of
    MacOS -> do
      liftIO $ putStrLn "Restarting Nix daemon (macOS)..."
      void $ cmd (exe "sudo" "launchctl" "unload" "/Library/LaunchDaemons/org.nixos.nix-daemon.plist")
      void $ cmd (exe "sudo" "launchctl" "load" "/Library/LaunchDaemons/org.nixos.nix-daemon.plist")
    Debian -> do
      liftIO $ putStrLn "Restarting Nix daemon (Debian)..."
      void $ cmd (exe "sudo" "systemctl" "restart" "nix-daemon.service")
    Unknown -> error "Cannot restart nix daemon: unknown OS"
  liftIO $ threadDelay 5000000
