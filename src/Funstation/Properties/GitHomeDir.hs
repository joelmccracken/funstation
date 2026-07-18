{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Funstation.Properties.GitHomeDir where

import Control.Monad.Except (throwError)
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Funstation.Properties.HasGit (HasGitP(..))
import Shh (exe, captureTrim, (|>), Proc, Failure)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless, void, forM_, forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)


-- | Build a git command scoped to a bare git dir.
gitDirCmd :: Text -> [String] -> Proc ()
gitDirCmd dir args = exe $ ["git", "--git-dir", T.unpack dir] ++ args

-- | Get the remote URL for a named remote in a bare git dir.
gitDirRemoteUrl ::
  (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> Text -> m (Either Failure Text)
gitDirRemoteUrl gitDir name = do
  let theCmd =
        ["git", "--git-dir", gitDir, "remote", "get-url", name]
  result <- runCmd theCmd (|> captureTrim)
  pure $ fmap (TL.toStrict . TL.decodeUtf8) result

-- | List all tracked file paths under a treeish in a bare git dir.
gitLsTree :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> Text -> m (Either Failure [Text])
gitLsTree gitDir treeish = do
  let theCmd =
        ["git", "--git-dir", gitDir, "ls-tree", "-r", "--full-tree", "--name-only", treeish]
  result <- runCmd theCmd (|> captureTrim)
  pure $ fmap (filter (not . T.null) . T.lines . TL.toStrict . TL.decodeUtf8) result


-- | resolveGitDir
-- If gitDir path is absolute, use the exact path.
-- Otherwise, treate the gitDir as if it were relative to the home dir
-- example: `gitDir "/home/user" ".git-dir"    -->   "/home/user/.git-dir`
-- example: `gitDir "/home/anywhere" "/tmp/git-dir"    -->  "/tmp/git-dir"
resolveGitDir :: Text -> Text -> Text
resolveGitDir homeDir gitDir
  | "/" `T.isPrefixOf` gitDir = gitDir
  | otherwise                 = homeDir <> "/" <> gitDir

data GitHomeDirP = GitHomeDirP
  { gitDir         :: Text        -- ^ path to bare git dir, e.g. "~/.git-home"
  , remoteUrl      :: Text        -- ^ remote URL to fetch from
  , branch         :: Text        -- ^ branch name, e.g. "main"
  , homeDir        :: Maybe Text  -- ^ home dir work tree; defaults to "~" if Nothing
  , runAfterChange :: Maybe Text  -- ^ optional script run after any changes are made
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitHomeDirP where
  desc _ = "git home dir clone"
  attrs p = Map.fromList
    [ ("gitDir",    p.gitDir)
    , ("remoteUrl", p.remoteUrl)
    , ("branch",    p.branch)
    , ("homeDir",   tshow p.homeDir)
    ]

  checker p = do
    expandedHomeDir <- expandPath (fromMaybe "~" p.homeDir)
    expandedGitDir <- resolveGitDir expandedHomeDir <$> expandPath p.gitDir

    -- 1. Git dir must exist
    gitDirExists <- dirExists expandedGitDir
    if not gitDirExists then pure False else do
      -- 2. Remote URL must match
      hasRemote <- hasRemoteUrl expandedGitDir p.remoteUrl
      if not hasRemote then pure False else
        -- 3. All tracked files must be present in homeDir
        allTrackedFilesPresent expandedGitDir expandedHomeDir p.branch

   where
    hasRemoteUrl gitDir correctUrl =  do
      remoteResult <- gitDirRemoteUrl gitDir "origin"
      pure $ either (const False) (== correctUrl) remoteResult

    allTrackedFilesPresent gitDir destDir branch = do
      lsResult <- gitLsTree gitDir ("origin/" <> branch)
      either (const $ pure False) checkAllFiles lsResult
     where
        checkAllFiles files =
          fmap and $ forM files $ \f -> do
            let destPath = destDir <> "/" <> f
            fileExists destPath

  fixer p = do
    expandedHomeDir <- expandPath (fromMaybe "~" p.homeDir)
    expandedGitDir <- resolveGitDir expandedHomeDir <$> expandPath p.gitDir

    gitDirExists <- dirExists expandedGitDir

    didInitializeGitDir <-
      if not gitDirExists then do
        result <- runCmd ["git", "init", "--bare", expandedGitDir] id
        case result of
          Right _ -> do
            void $ cmd $ exe $ T.encodeUtf8 <$>
              ["git", "--git-dir", expandedGitDir, "config", "core.bare", "false"]
            void $ cmd $ exe $ T.encodeUtf8 <$>
              ["git", "--git-dir", expandedGitDir, "config", "core.worktree", expandedHomeDir]
            putStrLn' $ "Initialized repo at " <> expandedGitDir
            pure True
          Left err -> throwError $ WSFailure $ "Failed to init bare repo: " <> tshow err
      else pure False

    -- Step 2: configure remote (add or update)
    remoteResult <- gitDirRemoteUrl expandedGitDir "origin"
    didSetRemoteUrl <- case remoteResult of
      Right currentUrl | currentUrl == p.remoteUrl ->
        pure False -- already correct
      Right _ -> do
        -- TODO this branch needs to be tested
        void $ runCmd ["git", "--git-dir", expandedGitDir, "--work-tree", expandedHomeDir, "remote", "set-url", "origin", p.remoteUrl] id
        pure True
      Left _ -> do
        -- TODO ensure this branch tested
        void $ runCmd ["git", "--git-dir", expandedGitDir, "remote", "add", "origin", p.remoteUrl] id
        pure True

    -- Step 3: fetch (always; errors out on failure)
    fetchResult <- runCmd ["git", "--git-dir", expandedGitDir, "fetch", "origin"] id
    case fetchResult of
      Left err -> throwError $ WSFailure $ "Failed to fetch from remote: " <> tshow err
      Right _  -> pure ()

    -- Step 4: ensure local branch exists and tracks remote
    let remoteBranch = "origin/" <> p.branch
    void $ cmd $ exe $ T.encodeUtf8 <$>
      ["git", "--git-dir", expandedGitDir, "branch", p.branch, remoteBranch]
    void $ runCmd ["git", "--git-dir", expandedGitDir, "branch", "--set-upstream-to", remoteBranch, p.branch] id

    -- Switch HEAD to the correct branch and reset index to match it
    void $ cmd $ exe $ T.encodeUtf8 <$>
      ["git", "--git-dir", expandedGitDir, "symbolic-ref", "HEAD", "refs/heads/" <> p.branch]
    void $ cmd $ exe $ T.encodeUtf8 <$>
      ["git", "--git-dir", expandedGitDir, "read-tree", p.branch]

    -- Step 5: ensure homeDir exists

    homeDirExists <- dirExists expandedHomeDir
    unless homeDirExists $ mkDir expandedHomeDir

    -- Step 6: soft-checkout files missing from homeDir
    lsResult <- gitLsTree expandedGitDir ("origin/" <> p.branch)
    didChangeFiles <- case lsResult of
      Left err -> throwError $ WSFailure $ "Failed to list tracked files: " <> tshow err
      Right files -> do
        forM files $ \f -> do
          let destPath = expandedHomeDir <> "/" <> f
          exists <- fileExists destPath
          if exists then pure False else do
            putStrLn' $ "Checking out missing file: " <> f
            result <- runCmd ["bash", "-c", T.intercalate " "
                                [ "cd ", expandedHomeDir, ";"
                                , "git", "--git-dir", expandedGitDir
                                , "--work-tree", expandedHomeDir
                                , "checkout", "origin/" <> p.branch, "--", f]] id
            case result of
              Right _ -> pure True
              Left err -> do
                putStrLn' $ "Warning: failed to checkout " <> f <> ": " <> tshow err
                pure False

    -- Step 7: run post-change script if anything changed
    let didChange = (or [didInitializeGitDir, didSetRemoteUrl, any (==True) didChangeFiles])
    when didChange  $ forM_ p.runAfterChange $ \script -> do
      expandedScript <- expandPath script
      putStrLn' $ "Running post-change script: " <> expandedScript
      result <- runCmd ["bash", expandedScript] id
      case result of
        Right _ -> pure ()
        Left err -> throwError $ WSFailure $ "Post-change script failed: " <> tshow err

  dependencies _ = return [IsProp HasGitP]
