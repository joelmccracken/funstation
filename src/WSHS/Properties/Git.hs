{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WSHS.Properties.Git where

import Control.Monad.Except (throwError)
import WSHS.Types
import WSHS.Commands
import WSHS.Properties.MacOS
import WSHS.Properties.Debian
import Shh (exe, devNull, (&>))
import Data.Text.Encoding qualified as T
import Data.Bool (bool)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)


data HasGitP = HasGitP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GitMacOSP = GitMacOSP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GitDebianP = GitDebianP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop GitMacOSP where
  desc _ = "git (macOS via Homebrew)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    args' <- mkWSCmd ["brew", "install", "git"]
    result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = return [(IsProp HomebrewP)]

instance Prop GitDebianP where
  desc _ = "git (Debian via apt)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    args' <- mkWSCmd ["sudo", "apt-get", "install", "-y", "git"]
    result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = do
    hasCmd' "git" >>= bool (return [(IsProp AptUpdateP)]) (return [])

instance Prop HasGitP where
  desc _ = "has command `git` installed"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  dependencies _ = do
    os <- detectOS
    case os of
      MacOS -> return [(IsProp HomebrewP)]
      Debian -> return [(IsProp AptUpdateP)]
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- detectOS
    case os of
      Unknown -> throwError $ WSFailure "error: Unknown OS, unable to install git"
      MacOS -> do
        args' <- mkWSCmd ["brew", "install", "git"]
        result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> throwError $ WSFailure $ "Failed to install git: " <> tshow err
      Debian -> do
        args' <- mkWSCmd ["sudo", "apt-get", "install", "-y", "git"]
        result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> throwError $ WSFailure $ "Failed to install git: " <> tshow err
