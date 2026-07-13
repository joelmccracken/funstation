{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.Basic where

import WSHS.Types
import WSHS.Properties.HasGit
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data BasicSetupP = BasicSetupP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop BasicSetupP where
  desc _ = "basic setup"
  attrs _ = mempty
  checker _ = return True -- dummy prop, wrapper for dependencies
  fixer _ = return () -- all action in dependencies
  dependencies _ = return [(IsProp HasGitP)]
