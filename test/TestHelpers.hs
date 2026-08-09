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
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket_)

import Funstation.Types

-- | Run an action with a fresh temporary directory, named with the given
-- prefix. Partially apply the prefix to get a suite-local @withTempDir@.
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory

-- | Build a minimal Settings for tests, parameterised by sudoCmd and detected OS.
mkSettings :: String -> OS -> Settings
mkSettings sc o =
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = Nothing, workstation = Nothing }
  in Settings { opts = opts, sudoCmd = sc, workstation = "workstation", os = o }

-- | Run a WS action with a minimal configuration.
runWS :: WS a -> IO a
runWS = runWSWith "sudo"

-- | Run a WS action with a specific sudoCmd in Settings.
runWSWith :: String -> WS a -> IO a
runWSWith sc = runWSFull sc MacOS

-- | Run a WS action with a specific detected OS in Settings.
runWSWithOS :: OS -> WS a -> IO a
runWSWithOS = runWSFull "sudo"

-- | Core runner: given sudoCmd and OS, run the action and turn 'WSError's into
-- test failures.
runWSFull :: String -> OS -> WS a -> IO a
runWSFull sc o action = do
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) (mkSettings sc o))) initialState

-- | Run a WS action with the given OS, returning an 'Either' to handle errors
runWSEither :: OS -> WS a -> IO (Either WSError a)
runWSEither o action = do
  let initialState = WSState { props = Set.empty }
  fst <$> runStateT (runExceptT (runReaderT (unWS action) (mkSettings "sudo" o))) initialState

-- | Temporarily set a file's mode, restoring the original after the action.
-- Ensures cleanup can proceed even if the action throws.
withFileMode :: FilePath -> Int -> IO a -> IO a
withFileMode path newMode action =
  bracket_
    (setFileMode path (fromIntegral newMode))
    (setFileMode path 0o644)
    action

failLeft :: Either WSError a -> IO a
failLeft result =
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Left WSAborted -> fail "WS action aborted"
    Right a -> pure a

-- | Run a monadic action and assert its result equals the expected value.
shouldBeM :: (Show a, Eq a) => a -> IO a -> Expectation
shouldBeM expected op = do
  res <- op
  res `shouldBe` expected

-- | Run a monadic action and assert its result satisfies the given predicate.
shouldSatisfyM :: Show a => (a -> Bool) -> IO a -> Expectation
shouldSatisfyM p op = do
  res <- op
  res `shouldSatisfy` p
