{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TestHelpers where

import Test.Hspec
import Data.Text qualified as T
import Data.Set qualified as Set
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import System.Posix.Files (setFileMode)
import Control.Exception (bracket_)

import Funstation.Types
import Funstation.Properties.GitHomeDir (GitHomeDirP (..))

-- | Run a WS action with a minimal configuration.
runWS :: WS a -> IO a
runWS = runWSWith "sudo"

-- | Run a WS action with a specific sudoCmd in Settings.
runWSWith :: String -> WS a -> IO a
runWSWith sc action = do
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = "", workstation = Nothing }
  let settings = Settings { opts = opts, sudoCmd = sc, workstation = "workstation" }
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) settings)) initialState

-- | Temporarily set a file's mode, restoring the original after the action.
-- Ensures cleanup can proceed even if the action throws.
withFileMode :: FilePath -> Int -> IO a -> IO a
withFileMode path newMode action =
  bracket_
    (setFileMode path (fromIntegral newMode))
    (setFileMode path 0o644)
    action

failLeft result =
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Right a -> pure a

-- | Run a monadic action and assert its result equals the expected value.
shouldBeM :: (Show a, Eq a) => a -> IO a -> Expectation
shouldBeM expected op = do
  res <- op
  res `shouldBe` expected
