{-# LANGUAGE OverloadedStrings #-}

module Funstation.Utility (module Funstation.Utility) where

import Data.Text qualified as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (exitFailure, exitSuccess)

import Funstation.Types

-- | Unwrap the result of a WS run, reporting failures to the user and exiting.
-- A 'WSFailure' prints the error and exits with failure; a 'WSAborted' reports
-- the abort and exits successfully.
failLeft :: MonadIO m => Either WSError a -> m a
failLeft = either (liftIO . handleFail) pure
 where
  handleFail (WSFailure msg) = do
    putStrLn $ "fun: error: " <> T.unpack msg
    exitFailure
  handleFail WSAborted = do
    putStrLn "Run aborted."
    exitSuccess
