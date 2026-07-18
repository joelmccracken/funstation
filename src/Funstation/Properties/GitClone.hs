{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Funstation.Properties.GitClone (GitCloneP(..)) where

import Control.Monad.Except (throwError)
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Funstation.Properties.HasGit (HasGitP(..))
import Funstation.Properties.GitHomeDir (gitDirRemoteUrl)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

-- | A simple, no-fuss @git clone@ of a repo to a path.
data GitCloneP = GitCloneP
  { repoUrl :: Text        -- ^ remote URL to clone
  , path    :: Text        -- ^ destination path (supports ~ expansion)
  , branch  :: Maybe Text  -- ^ optional branch; default branch if Nothing
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitCloneP where
  desc _ = "git clone"
  attrs p = Map.fromList
    [ ("repoUrl", p.repoUrl)
    , ("path",    p.path)
    ] <> maybe mempty (Map.singleton "branch") p.branch

  checker p = do
    expandedPath <- expandPath p.path
    exists <- dirExists expandedPath
    if not exists then pure False else do
      result <- gitDirRemoteUrl (expandedPath <> "/.git") "origin"
      pure $ either (const False) (== p.repoUrl) result

  fixer p = do
    expandedPath <- expandPath p.path
    let branchArgs = maybe [] (\b -> ["--branch", b]) p.branch
    result <- runCmd (["git", "clone"] ++ branchArgs ++ [p.repoUrl, expandedPath]) id
    case result of
      Right _  -> putStrLn' $ "Cloned " <> p.repoUrl <> " to " <> p.path
      Left err -> throwError $ WSFailure $ "Failed to clone repository: " <> tshow err

  dependencies _ = return [IsProp HasGitP]
