{-# LANGUAGE OverloadedStrings #-}

-- | The @~/.local/state/funstation@ directory.
--
-- Information on previous runs, used by Properties.
module Funstation.State where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Funstation.Commands (readFileIfExists)

-- | Root of the state directory, given a home directory.
stateDir :: FilePath -> FilePath
stateDir home = home </> ".local" </> "state" </> "funstation"

-- | A file inside the state directory, named by a path relative to its root.
stateFile :: FilePath -> FilePath -> FilePath
stateFile home rel = stateDir home </> rel

-- | Read a state file's contents, stripped; 'Nothing' when it does not exist.
readStateFile :: MonadIO m => FilePath -> m (Maybe Text)
readStateFile = readFileIfExists

-- | Write a state file.
-- A trailing newline is added to file contents so the
-- files stay readable with @cat@.
writeStateFile :: MonadIO m => FilePath -> Text -> m ()
writeStateFile path contents = liftIO $ do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (contents <> "\n")

-- | Read a POSIX timestamp previously recorded by 'writeTimestamp'.
--
-- Returns 0 if timestamp cannot be read from file.
readTimestamp :: MonadIO m => FilePath -> m Integer
readTimestamp path = do
  contents <- readStateFile path
  pure $ fromMaybe 0 (readMaybe . T.unpack =<< contents)

-- | Record the current timestamp in state file.
writeTimestamp :: MonadIO m => FilePath -> m ()
writeTimestamp path = do
  now <- liftIO getPOSIXTime
  writeStateFile path (T.pack (show (round now :: Integer)))
