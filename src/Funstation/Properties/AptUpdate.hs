{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.AptUpdate where

import Funstation.Types
import Funstation.Commands
import Funstation.State
import Funstation.Proc
import Shh (devNull, (&>))
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.IO.Class (MonadIO, liftIO)

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Re-run @apt-get update@ only if last run over 1 day ago
aptUpdateIntervalSeconds :: Integer
aptUpdateIntervalSeconds = 60 * 60 * 24

-- | path to last successful @apt-get update@ time.
aptUpdateTsFile :: FilePath -> FilePath
aptUpdateTsFile home = stateFile home ("apt-update" </> "last-run-ts")

-- | Read the last-run POSIX timestamp; 0 when never run.
getLastAptUpdateTs :: MonadIO m => FilePath -> m Integer
getLastAptUpdateTs = readTimestamp . aptUpdateTsFile

saveLastAptUpdateTs :: MonadIO m => FilePath -> m ()
saveLastAptUpdateTs = writeTimestamp . aptUpdateTsFile

instance Prop AptUpdateP where
  desc _ = "apt package lists updated"
  attrs _ = mempty

  -- Fresh iff last successful update within interval.
  checker _ = do
    home    <- liftIO $ getEnv "HOME"
    now     <- liftIO $ round <$> getPOSIXTime
    lastRun <- liftIO $ getLastAptUpdateTs home
    return $ (lastRun + aptUpdateIntervalSeconds) >= now

  fixer _ = do
    result <- runCmd ["sudo", "apt-get", "update"] (&> devNull)
    case result of
      Right _ -> do
        home <- liftIO $ getEnv "HOME"
        liftIO $ saveLastAptUpdateTs home
        putStrLn' "apt package sets updated successfully"
      -- Don't record a timestamp on failure, next run should retry.
      Left err -> putStrLn' $ "Failed to update apt: " <> tshow err

  dependencies _ = return []
