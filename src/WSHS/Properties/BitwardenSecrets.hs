{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module WSHS.Properties.BitwardenSecrets where

import WSHS.Types
import WSHS.Commands
import Shh (exe, captureTrim, (|>), Failure)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import System.Environment (getEnv, setEnv, unsetEnv)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (forM_, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)

-- ---------------------------------------------------------------------------
-- Data type

data BitwardenSecretsP = BitwardenSecretsP
  { syncIntervalDays :: Int  -- ^ How many days between re-syncs (default 7)
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- JSON types for bw CLI output

data BwFolder = BwFolder { id :: Text, name :: Text }
  deriving (Generic, FromJSON)

data BwItem = BwItem
  { id    :: Text
  , name  :: Text
  , notes :: Maybe Text
  } deriving (Generic, FromJSON)

-- ---------------------------------------------------------------------------
-- Helpers

-- | Run a bw command via bash, suppressing Node.js deprecation warnings.
bwCmd :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => [Text] -> m (Either Failure LBS.ByteString)
bwCmd args = do
  let shellCmd = "NODE_OPTIONS='--no-deprecation' bw " <> T.unwords args
  args' <- mkWSCmd ["bash", "-c", shellCmd]
  cmd $ exe (T.encodeUtf8 <$> args') |> captureTrim

-- | Read the last-sync POSIX timestamp from state file; returns 0 if absent.
getLastSyncTs :: FilePath -> IO Integer
getLastSyncTs home = do
  let tsFile = home <> "/.local/state/wshs/bitwarden-secrets/last-sync-ts"
  exists <- doesFileExist tsFile
  if not exists
    then return 0
    else do
      content <- readFile tsFile
      return $ read $ T.unpack $ T.strip $ T.pack content

-- | Persist the current POSIX time as the last-sync timestamp.
saveLastSyncTs :: FilePath -> IO ()
saveLastSyncTs home = do
  let dir    = home <> "/.local/state/wshs/bitwarden-secrets"
      tsFile = dir <> "/last-sync-ts"
  createDirectoryIfMissing True dir
  now <- round <$> getPOSIXTime
  writeFile tsFile (show (now :: Integer) <> "\n")

-- ---------------------------------------------------------------------------
-- Prop instance

instance Prop BitwardenSecretsP where
  desc _ = "Bitwarden secrets sync"
  attrs p = Map.fromList [("syncIntervalDays", T.pack (show p.syncIntervalDays))]

  -- True iff bw is installed, creds exist, and last sync is still fresh.
  checker p = do
    bwInstalled <- hasCmd' "bw"
    if not bwInstalled then return False
    else do
      home <- liftIO $ getEnv "HOME"
      let emailFile = home <> "/secrets/bw_email"
          passFile  = home <> "/secrets/bw_master_pass"
      emailExists <- liftIO $ doesFileExist emailFile
      passExists  <- liftIO $ doesFileExist passFile
      if not (emailExists && passExists) then return False
      else do
        now      <- liftIO $ round <$> getPOSIXTime
        lastSync <- liftIO $ getLastSyncTs home
        let interval = fromIntegral p.syncIntervalDays * 60 * 60 * 24 :: Integer
        return $ (lastSync + interval) >= now

  fixer _ = do
    home <- liftIO $ getEnv "HOME"
    let emailFile = home <> "/secrets/bw_email"
        passFile  = T.pack $ home <> "/secrets/bw_master_pass"

    -- 1. Login (silently ignored if already logged in)
    putStrLn' "  Logging in to Bitwarden..."
    email <- liftIO $ T.strip . T.pack <$> readFile emailFile
    _ <- bwCmd ["login", email, "--passwordfile", passFile]

    -- 2. Unlock vault, capture session token
    putStrLn' "  Unlocking Bitwarden vault..."
    tokenResult <- bwCmd ["unlock", "--passwordfile", passFile, "--raw"]
    token <- case tokenResult of
      Left err -> throwError $ WSFailure $ "bw unlock failed: " <> tshow err
      Right t  -> return $ TL.unpack $ TL.decodeUtf8 t

    -- Set BW_SESSION for all subsequent bw calls.
    -- TODO: use bracket-style cleanup so unsetEnv runs even on exception.
    liftIO $ setEnv "BW_SESSION" token

    -- 3. Sync vault
    putStrLn' "  Syncing Bitwarden vault..."
    syncResult <- bwCmd ["sync"]
    case syncResult of
      Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "bw sync failed: " <> tshow err }
      Right _  -> return ()

    -- 4. Find or create bww_files folder
    foldersResult <- bwCmd ["list", "folders", "--search", "bww_files"]
    folderId <- case foldersResult of
      Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "bw list folders failed: " <> tshow err }
      Right bs -> case eitherDecode bs :: Either String [BwFolder] of
        Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "Failed to parse folders JSON: " <> T.pack err }
        Right folders -> case filter (\f -> f.name == "bww_files") folders of
          (f:_) -> return f.id
          [] -> do
            putStrLn' "  Creating bww_files folder in Bitwarden vault..."
            let script = "NODE_OPTIONS='--no-deprecation' printf '%s' '{\"name\":\"bww_files\"}' | bw encode | bw create folder"
            createArgs <- mkWSCmd ["bash", "-c", script]
            createResult <- cmd $ exe (T.encodeUtf8 <$> createArgs) |> captureTrim
            case createResult of
              Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "bw create folder failed: " <> tshow err }
              Right bs2 -> case eitherDecode bs2 :: Either String BwFolder of
                Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "Failed to parse folder creation response: " <> T.pack err }
                Right f  -> return f.id

    -- 5. List items in the folder
    itemsResult <- bwCmd ["list", "items", "--folderid", folderId]
    items <- case itemsResult of
      Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "bw list items failed: " <> tshow err }
      Right bs -> case eitherDecode bs :: Either String [BwItem] of
        Left err -> do { liftIO (unsetEnv "BW_SESSION"); throwError $ WSFailure $ "Failed to parse items JSON: " <> T.pack err }
        Right is -> return is

    -- 6. Write each file item to disk
    let fileItems = filter (\i -> T.isPrefixOf "file:" i.name) items
    putStrLn' $ "  Found " <> T.pack (show (length fileItems)) <> " file item(s) in vault"
    forM_ fileItems $ \item -> do
      let rawPath = T.drop (T.length "file:") item.name
      expandedPath <- expandPath rawPath
      case item.notes of
        Nothing      -> putStrLn' $ "  Skipping " <> expandedPath <> " (no content)"
        Just content -> do
          putStrLn' $ "  Writing " <> expandedPath <> "..."
          ensureParentDir expandedPath
          void $ fileContentsFix expandedPath content
          chmodArgs <- mkWSCmd ["chmod", "0600", expandedPath]
          void $ cmd $ exe $ T.encodeUtf8 <$> chmodArgs

    -- 7. Save sync timestamp
    liftIO $ saveLastSyncTs home
    liftIO $ unsetEnv "BW_SESSION"
    putStrLn' "  Bitwarden secrets synced successfully."

  dependencies _ = return []
