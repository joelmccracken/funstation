{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Funstation.Commands where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (intToDigit)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import Control.Monad (void, unless, when)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, asks)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shh (exe, devNull, (&>), capture, captureTrim, (|>), Failure)
import Control.Monad.Except (MonadError, throwError)
import Funstation.Types
import Funstation.Sudo
import Funstation.Proc

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
  nixosCheck <- cmd (exe "test" "-f" "/etc/NIXOS" &> devNull)
  case nixosCheck of
    Right _ -> return NixOS
    Left _ -> do
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

-- | Read file contents, stripped of surrounding whitespace, or 'Nothing'
-- when the file does not exist.
readFileIfExists :: MonadIO m => FilePath -> m (Maybe Text)
readFileIfExists path = liftIO $ do
  exists <- doesFileExist path
  if exists
    then Just . T.strip <$> TIO.readFile path
    else pure Nothing

-- | Create a directory (and any missing parents).
mkDir :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
mkDir path = do
  void $ runCmd ["mkdir", "-p", path] id


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
getOwnerGroup :: MonadIO m => Text -> m Text
getOwnerGroup path = do
  result <- cmd (exe "ls" "-ld" (T.unpack path) |> captureTrim)
  case result of
    Left _ -> pure "root:root"
    Right bytes ->
      case words $ TL.unpack $ TL.decodeUtf8 bytes of
        (_:_:owner:group:_) -> pure $ T.pack owner <> ":" <> T.pack group
        _ -> pure "root:root"

-- | Get the octal permission mode (e.g. "644") for a path, for use with chmod.
getMode :: MonadIO m => Text -> m Text
getMode path = do
  result <- cmd (exe "ls" "-ld" (T.unpack path) |> captureTrim)
  case result of
    Left _ -> pure "644"
    Right bytes ->
      case words $ TL.unpack $ TL.decodeUtf8 bytes of
        (perms:_) -> pure $ permsToOctal (T.pack perms)
        _ -> pure "644"

-- | Convert an @ls -l@ style permission string (e.g. "-rw-r--r--") to an
-- octal mode string (e.g. "644"). Setuid/setgid/sticky markers are treated
-- the same as executable bit; fine for simple files
permsToOctal :: Text -> Text
permsToOctal perms = T.pack $ map triadToDigit (chunksOf3 rwx)
  where
    rwx = T.unpack $ T.drop 1 perms  -- drop the leading file-type character
    chunksOf3 [] = []
    chunksOf3 cs = take 3 cs : chunksOf3 (drop 3 cs)
    triadToDigit triad = intToDigit $ sum
      $ map snd $ filter ((/= '-') . fst) $ zip triad [4, 2, 1 :: Int]

-- | Move a file to a timestamped backup, using sudo only if needed.
mvToBackupAuto :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m Text
mvToBackupAuto path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  -- rename; write is to the directory, not files themselves
  result <- privCmdFor [(EntryAccess, path), (EntryAccess, backupPath)]
              ["mv", path, backupPath]
  case result of
    Right _ -> do
      putStrLn' $ "  Backed up " <> path <> " to " <> backupPath
      pure backupPath
    Left err -> throwError $ WSFailure $ "Failed to backup file: " <> tshow err

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

-- | Options for 'fileContentsFixWith'.
data FixOpts m = FixOpts
  { validate :: Maybe (FilePath -> m Bool)
    -- ^ Run on the temp file holding the new contents
  , newMode  :: Maybe Text
    -- ^ Octal mode for new files (default @"644"@)
  }

defaultFixOpts :: FixOpts m
defaultFixOpts = FixOpts { validate = Nothing, newMode = Nothing }

-- | Ensure a file has the desired contents.
-- Returns Nothing if no change was needed, Just backupPath if the file was updated.
-- The backupPath will be empty string if no backup was needed (file didn't exist).
-- TODO think about what parts to reuse/share (e.g. with dotfiles code)
fileContentsFix :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> Text -> m (Maybe Text)
fileContentsFix = fileContentsFixWith defaultFixOpts

-- | 'fileContentsFix', with validation and new-file mode controlled by 'FixOpts'.
fileContentsFixWith :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => FixOpts m -> Text -> Text -> m (Maybe Text)
fileContentsFixWith opts path content = do
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
              tempFileT = T.pack tempFile

          -- Write desired content to temp file
          liftIO $ TIO.writeFile tempFile content

          -- Validate the new contents before touching the target
          case opts.validate of
            Nothing -> pure ()
            Just v -> do
              valid <- v tempFile
              unless valid $ do
                void $ cmd $ exe ["rm", "-f", tempFile]
                throwError $ WSFailure $ "New contents for " <> path <> " failed validation; left unchanged"

          -- Check if target exists; capture owner/mode info before any changes
          targetExists <- fileExists path
          (ownerGroup, mode) <- if targetExists
            then (,) <$> getOwnerGroup path <*> getMode path
            else (,) <$> getOwnerGroup (T.pack $ takeDirectory (T.unpack path)) <*> pure (fromMaybe "644" opts.newMode)

          -- Give the temp file the target's mode and ownership before moving
          -- it in, so the target never exists with the wrong ones
          tempMode <- getMode tempFileT
          when (tempMode /= mode) $
            void $ privCmd ModeAccess tempFileT ["chmod", mode, tempFileT]
          tempOwnerGroup <- getOwnerGroup tempFileT
          when (tempOwnerGroup /= ownerGroup) $
            void $ privCmd OwnerAccess tempFileT ["chown", ownerGroup, tempFileT]

          -- Back up existing file if present
          backupPath <- if targetExists
            then mvToBackupAuto path
            else pure ""

          -- Move temp file to target location
          -- may need owner change, so check EntryAccess, etc
          moveResult <- privCmdFor [(EntryAccess, tempFileT), (EntryAccess, path), (ModeAccess, tempFileT)]
                          ["mv", tempFileT, path]
          case moveResult of
            Left err -> throwError $ WSFailure $ "Failed to move file to " <> path <> ": " <> tshow err
            Right _ -> pure ()

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

-- | Expand variables in multi-line text (e.g. file contents) with bash, by
-- feeding it through an unquoted heredoc.
--
-- Unlike 'expandPath', the text is not parsed as a command line: parens,
-- newlines, comments, stars, and whitespace are kept as written; only
-- @$VAR@, @${VAR}@, @$(...)@, backticks, and backslashes are interpreted.
-- Referencing an unset variable is an error (@set -u@), rather than 
-- expanding to nothing.
--
-- Caveats:
--
-- * a literal @$@ must be written @\\$@, and a literal @\\@ as @\\\\@
-- * a line consisting of exactly @FUNSTATION_EOF@ ends the heredoc early
-- * expansion runs as the invoking user, never via sudo
expandText :: (MonadIO m, MonadError WSError m) => Text -> m Text
expandText text = do
  -- TODO I really should figure out a better way to handle interpolations etc
  -- the heredoc always ends its output with a newline; match the input
  let endsInNewline = "\n" `T.isSuffixOf` text
      body = if endsInNewline then text else text <> "\n"
      script = "set -u\ncat <<FUNSTATION_EOF\n" <> body <> "FUNSTATION_EOF\n"
  result <- cmd (exe "bash" "-c" (T.unpack script) |> capture)
  case result of
    Left err -> throwError $ WSFailure $ "Failed to expand text: " <> tshow err
    Right bytes -> do
      let expanded = TL.toStrict $ TL.decodeUtf8 bytes
      pure $ if endsInNewline then expanded else fromMaybe expanded (T.stripSuffix "\n" expanded)

mvToBackup :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
mvToBackup path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- runCmd ["mv", path, backupPath] (&> devNull)
  case result of
    Right _ -> putStrLn' $ "Moved " <> path <> " to " <> backupPath
    Left err -> throwError $ WSFailure $ "Failed to move file: " <> tshow err

-- | Install a package via Homebrew (@brew install \<package\>@).
brewInstall :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m (Either Failure ())
brewInstall package = runCmd ["brew", "install", package] (&> devNull)

-- | Install a package via apt (@sudo apt-get install -y \<package\>@).
aptInstall :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m (Either Failure ())
aptInstall package = runCmd ["sudo", "apt-get", "install", "-y", package] (&> devNull)

-- | Restart the Nix daemon (OS-aware)
restartNixDaemon :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => m ()
restartNixDaemon = do
  os <- asks (.os)
  case os of
    MacOS -> do
      liftIO $ putStrLn "Restarting Nix daemon (macOS)..."
      void $ runCmd ["sudo", "launchctl", "unload", "/Library/LaunchDaemons/org.nixos.nix-daemon.plist"] id
      void $ runCmd ["sudo", "launchctl", "load", "/Library/LaunchDaemons/org.nixos.nix-daemon.plist"] id
    Debian -> systemdRestart "Debian"
    NixOS -> systemdRestart "NixOS"
    Unknown -> throwError $ WSFailure "Cannot restart nix daemon: unknown OS"
  liftIO $ threadDelay 5000000
 where
  systemdRestart osName = do
    liftIO $ putStrLn $ "Restarting Nix daemon (" <> osName <> ")..."
    void $ runCmd ["sudo", "systemctl", "restart", "nix-daemon.service"] id
