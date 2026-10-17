{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TupleSections          #-}

module Funstation.Properties.BitwardenSecrets where

import Funstation.Types
import Funstation.Commands
import Funstation.State
import Funstation.Proc
import Shh (captureTrim, (|>), Failure)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import System.Environment (getEnv, setEnv, unsetEnv, lookupEnv)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Numeric (readOct)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (forM, unless, void)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)

-- ---------------------------------------------------------------------------
-- Data type

data BitwardenSecretsP = BitwardenSecretsP
  { syncIntervalDays :: Int         -- ^ How many days between re-syncs (default 7)
  , mode             :: Maybe Text  -- ^ Octal mode for written secrets (default 'defaultSecretMode')
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Mode written secrets get when the property sets none.
defaultSecretMode :: Text
defaultSecretMode = "700"

-- | The mode written secrets should have.
secretMode :: BitwardenSecretsP -> Text
secretMode p = fromMaybe defaultSecretMode p.mode

-- ---------------------------------------------------------------------------
-- JSON types for bw CLI output

data BwFolder = BwFolder { id :: Text, name :: Text }
  deriving (Eq, Show, Generic, FromJSON)

data BwItem = BwItem
  { id    :: Text
  , name  :: Text
  , notes :: Maybe Text
  } deriving (Eq, Show, Generic, FromJSON)

-- ---------------------------------------------------------------------------
-- Credentials

-- | Locations of the credentials the property needs, under @~/secrets@.
--
-- client id and client secret provide authentication, but they do not unlock the vault.
-- That requires the master password. All three files must be present.
data BwCredPaths = BwCredPaths
  { clientIdFile     :: FilePath
  , clientSecretFile :: FilePath
  , masterPassFile   :: FilePath
  } deriving (Eq, Show)

-- | Derive the credential paths from a home directory.
bwCredPaths :: FilePath -> BwCredPaths
bwCredPaths home = BwCredPaths
  { clientIdFile     = home <> "/secrets/bw_client_id"
  , clientSecretFile = home <> "/secrets/bw_client_secret"
  , masterPassFile   = home <> "/secrets/bw_master_pass"
  }

-- | True when every credential file is present on disk.
credsPresent :: BwCredPaths -> IO Bool
credsPresent creds = and <$> mapM doesFileExist credFiles
 where
   credFiles = [creds.clientIdFile, creds.clientSecretFile, creds.masterPassFile]


-- | Read a credential file, failing with the file's name when it is missing or
-- empty rather than letting bw emit a confusing error further along.
readCredFile :: (MonadIO m, MonadError WSError m) => FilePath -> m Text
readCredFile path = do
  contents <- readFileIfExists path
  case contents of
    Nothing -> wsFail $ "Missing Bitwarden credential file: " <> T.pack path
    Just c
      | T.null c  -> wsFail $ "Bitwarden credential file is empty: " <> T.pack path
      | otherwise -> return c

-- ---------------------------------------------------------------------------
-- Helpers

-- | Run a bw command via bash
--
-- Suppressing Node.js deprecation warnings, which interfere with output.
-- This runs unattended, so @--nointeraction@ is passed to every call,
-- which forces a failure instead of hanging.
bwCmd :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => [Text] -> m (Either Failure LBS.ByteString)
bwCmd args = do
  let shellCmd = "NODE_OPTIONS='--no-deprecation' bw --nointeraction " <> T.unwords args
  runCmd ["bash", "-c", shellCmd] (|> captureTrim)

-- | Set an environment variable for the duration of an action, then put the
-- environment back the way it was: the prior value is restored if there was
-- one, and the variable is unset if there was not.
withEnvVar :: (MonadIO m, MonadMask m) => String -> String -> m a -> m a
withEnvVar key value act =
  bracket acquire restore (const act)
  where
    acquire = liftIO $ do
      prior <- lookupEnv key
      setEnv key value
      pure prior
    restore = liftIO . maybe (unsetEnv key) (setEnv key)

withApiKeyEnv :: (MonadIO m, MonadMask m, MonadError WSError m) => BwCredPaths -> m a -> m a
withApiKeyEnv creds act = do
  clientId     <- readCredFile creds.clientIdFile
  clientSecret <- readCredFile creds.clientSecretFile
  withEnvVar "BW_CLIENTID" (T.unpack clientId) $
    withEnvVar "BW_CLIENTSECRET" (T.unpack clientSecret) act

-- | when vault was last synced.
syncTsFile :: FilePath -> FilePath
syncTsFile home = stateFile home ("bitwarden-secrets" </> "last-sync-ts")

-- | Read last-sync POSIX timestamp; 0 when never synced.
getLastSyncTs :: MonadIO m => FilePath -> m Integer
getLastSyncTs = readTimestamp . syncTsFile

-- | Persist current POSIX time as last-sync timestamp.
saveLastSyncTs :: MonadIO m => FilePath -> m ()
saveLastSyncTs = writeTimestamp . syncTsFile

-- | Paths of the files written by the last sync, one per line.
writtenFilesFile :: FilePath -> FilePath
writtenFilesFile home = stateFile home ("bitwarden-secrets" </> "written-files")

-- | Paths written by the last sync; empty when never synced.
getWrittenFiles :: MonadIO m => FilePath -> m [Text]
getWrittenFiles home =
  maybe [] (filter (not . T.null) . T.lines) <$> readStateFile (writtenFilesFile home)

-- | Record the paths written by a sync, replacing the previous list.
saveWrittenFiles :: MonadIO m => FilePath -> [Text] -> m ()
saveWrittenFiles home = writeStateFile (writtenFilesFile home) . T.unlines

-- | True when two octal mode strings name the same mode, so @"0700"@ and
-- @"700"@ compare equal. Unparseable modes never match.
sameMode :: Text -> Text -> Bool
sameMode a b = case (parse a, parse b) of
  (Just x, Just y) -> x == y
  _                -> False
  where
    parse :: Text -> Maybe Integer
    parse t = case readOct (T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

-- | True when every path exists and has the given mode.
filesHaveMode :: MonadIO m => Text -> [Text] -> m Bool
filesHaveMode wanted paths = and <$> mapM hasMode paths
  where
    hasMode path = do
      exists <- fileExists path
      if not exists then return False
      else sameMode wanted <$> getMode path

-- | Determines if @lastSync@ within @intervalDays@ of @now@.
-- All times are POSIX seconds.
isSyncFresh :: Int -> Integer -> Integer -> Bool
isSyncFresh intervalDays lastSync now =
  (lastSync + fromIntegral intervalDays * 60 * 60 * 24) >= now

-- ---------------------------------------------------------------------------
-- Vault contents

-- | Prefix marking a vault item as a file to be written to disk.
filePrefix :: Text
filePrefix = "file:"

-- | Name of the vault folder holding the file items.
filesFolderName :: Text
filesFolderName = "bww_files"

-- | Select the @file:@ items from a vault listing, pairing the destination path
-- (prefix stripped, still unexpanded), with the item's contents.
fileItemTargets :: [BwItem] -> [(Text, Maybe Text)]
fileItemTargets = mapMaybe target
  where
    target i = (, i.notes) <$> T.stripPrefix filePrefix i.name

parseFolders :: LBS.ByteString -> Either String [BwFolder]
parseFolders = eitherDecode

parseFolder :: LBS.ByteString -> Either String BwFolder
parseFolder = eitherDecode

parseItems :: LBS.ByteString -> Either String [BwItem]
parseItems = eitherDecode

-- | The id of the vault folder holding the file items, creating the folder if
-- the vault does not have one yet.
--
-- Requires an unlocked vault (i.e. @BW_SESSION@ already set).
findOrCreateFilesFolder :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => m Text
findOrCreateFilesFolder = do
  bs <- orFail "bw list folders failed" $ bwCmd ["list", "folders", "--search", filesFolderName]
  folders <- orFail' "Failed to parse folders JSON" $ parseFolders bs
  case filter (\f -> f.name == filesFolderName) folders of
    (f:_) -> return f.id
    []    -> createFilesFolder
  where
    createFilesFolder :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => m Text
    createFilesFolder = do
      putStrLn' $ "  Creating " <> filesFolderName <> " folder in Bitwarden vault..."
      -- Not routed through bwCmd because it pipes two bw calls together, so
      -- --nointeraction is spelled out on each.
      let script = "NODE_OPTIONS='--no-deprecation' printf '%s' '{\"name\":\"bww_files\"}' | bw --nointeraction encode | bw --nointeraction create folder"
      bs <- orFail "bw create folder failed" $ runCmd ["bash", "-c", script] (|> captureTrim)
      f <- orFail' "Failed to parse folder creation response" $ parseFolder bs
      return f.id

-- | Write each file item to disk with the given mode, returning the
-- (expanded) paths written.
writeFileItems :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> [(Text, Maybe Text)] -> m [Text]
writeFileItems fileMode targets =
  fmap catMaybes $ forM targets $ \(rawPath, mContent) -> do
    expandedPath <- expandPath rawPath
    case mContent of
      Nothing      -> do
        putStrLn' $ "  Skipping " <> expandedPath <> " (no content)"
        return Nothing
      Just content -> do
        putStrLn' $ "  Writing " <> expandedPath <> "..."
        ensureParentDir expandedPath
        -- newMode covers files created here; the chmod covers existing ones,
        -- whose mode fileContentsFix otherwise preserves.
        void $ fileContentsFixWith defaultFixOpts { newMode = Just fileMode } expandedPath content
        currentMode <- getMode expandedPath
        unless (sameMode fileMode currentMode) $
          orFail ("chmod " <> fileMode <> " " <> expandedPath <> " failed") $
            runCmd ["chmod", fileMode, expandedPath] Prelude.id
        return (Just expandedPath)

-- ---------------------------------------------------------------------------
-- Prop instance

instance Prop BitwardenSecretsP where
  desc _ = "Bitwarden secrets sync"
  attrs p = Map.fromList
    [ ("syncIntervalDays", T.pack (show p.syncIntervalDays))
    , ("mode", secretMode p)
    ]

  -- True iff bw is installed, creds exist, last sync is still fresh, and every
  -- file the last sync wrote still has the configured mode.
  checker p = do
    bwInstalled <- hasCmd' "bw"
    if not bwInstalled then return False
    else do
      home <- liftIO $ getEnv "HOME"
      let creds = bwCredPaths home
      credsExist <- liftIO $ credsPresent creds
      if not credsExist then return False
      else do
        now      <- liftIO $ round <$> getPOSIXTime
        lastSync <- liftIO $ getLastSyncTs home
        if not (isSyncFresh p.syncIntervalDays lastSync now) then return False
        else getWrittenFiles home >>= filesHaveMode (secretMode p)

  fixer p = do
    home <- liftIO $ getEnv "HOME"
    let creds = bwCredPaths home

    -- Log in with the personal API key.
    -- The result is deliberately ignored: bw exits non-zero when already logged in,
    -- and the unlock below is what actually matters.
    putStrLn' "  Logging in to Bitwarden (API key)..."
    _ <- withApiKeyEnv creds $ bwCmd ["login", "--apikey"]

    -- Unlock vault, capture session token
    -- session token is what acts like a "key" for the remaining commands
    putStrLn' "  Unlocking Bitwarden vault..."
    token <- TL.unpack . TL.decodeUtf8
      <$> orFail "bw unlock failed" (bwCmd ["unlock", "--passwordfile", T.pack creds.masterPassFile, "--raw"])

    -- Set BW_SESSION for all subsequent bw calls
    withEnvVar "BW_SESSION" token $ do
      -- Sync vault, make sure local data is fresh
      putStrLn' "  Syncing Bitwarden vault..."
      void $ orFail "bw sync failed" $ bwCmd ["sync"]

      folderId <- findOrCreateFilesFolder

      -- list all bww items
      itemsBs <- orFail "bw list items failed" $ bwCmd ["list", "items", "--folderid", folderId]
      items <- orFail' "Failed to parse items JSON" $ parseItems itemsBs

      -- save all bww items to disk
      let targets = fileItemTargets items
      putStrLn' $ "  Found " <> T.pack (show (length targets)) <> " file item(s) in vault"
      written <- writeFileItems (secretMode p) targets
      saveWrittenFiles home written

    -- save sync timestamp
    liftIO $ saveLastSyncTs home
    putStrLn' "  Bitwarden secrets synced successfully."

  dependencies _ = return []
