{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.Basic where

import WSHS.Types
import WSHS.Commands
import WSHS.Properties.Git
import Shh (exe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Either (isRight)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data BasicSetupP = BasicSetupP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data WSConfigDirP = WSConfigDirP
  { configDir        :: Text
  , configRepoUrl    :: Text
  , configRepoBranch :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop BasicSetupP where
  desc _ = "basic setup"
  attrs _ = mempty
  checker _ = return True -- dummy prop, wrapper for dependencies
  fixer _ = return () -- all action in dependencies
  dependencies _ = return [(IsProp HasGitP)]

instance Prop WSConfigDirP where
  desc _ = "wshs configuration directory"
  attrs _ = mempty
  checker p = do
    expandedDir <- expandPath p.configDir
    isRight <$> cmd (exe "test" "-d" (T.unpack expandedDir))
  fixer p = do
    expandedDir <- expandPath p.configDir
    result <- cmd (exe "git" "clone" "--branch" (T.unpack p.configRepoBranch) (T.unpack p.configRepoUrl) (T.unpack expandedDir))
    case result of
      Right _ -> putStrLn' $ "Cloned repository to " <> p.configDir
      Left err -> putStrLn' $ "Failed to clone repository: " <> tshow err
  dependencies _ = return [IsProp HasGitP]
