{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WSHS.Properties.Git where

import WSHS.Types
import WSHS.Commands
import WSHS.Properties.MacOS
import WSHS.Properties.Debian
import Shh (exe, devNull, (&>), Proc, Failure, captureTrim, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (when, unless, void, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)


-- | Build a git command scoped to a bare git dir.
gitDirCmd :: Text -> [String] -> Proc ()
gitDirCmd dir args = exe $ ["git", "--git-dir", T.unpack dir] ++ args

-- | Get the remote URL for a named remote in a bare git dir.
gitDirRemoteUrl :: Text -> Text -> WS (Either Failure Text)
gitDirRemoteUrl gitDir name = do
  result <- cmd (gitDirCmd gitDir ["remote", "get-url", T.unpack name] |> captureTrim)
  pure $ fmap (TL.toStrict . TL.decodeUtf8) result

-- | List all tracked file paths under a treeish in a bare git dir.
gitLsTree :: Text -> Text -> WS (Either Failure [Text])
gitLsTree gitDir treeish = do
  result <- cmd (gitDirCmd gitDir ["ls-tree", "-r", "--name-only", T.unpack treeish] |> captureTrim)
  pure $ fmap (filter (not . T.null) . T.lines . TL.toStrict . TL.decodeUtf8) result

data GitHomeDirCloneP = GitHomeDirCloneP
  { gitDir         :: Text        -- ^ path to bare git dir, e.g. "~/.git-home"
  , remoteUrl      :: Text        -- ^ remote URL to fetch from
  , branch         :: Text        -- ^ branch name, e.g. "main"
  , homeDir        :: Maybe Text  -- ^ home dir work tree; defaults to "~" if Nothing
  , runAfterChange :: Maybe Text  -- ^ optional script run after any changes are made
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitHomeDirCloneP where
  desc _ = "git home dir clone"
  attrs p = Map.fromList
    [ ("gitDir",    p.gitDir)
    , ("remoteUrl", p.remoteUrl)
    , ("branch",    p.branch)
    ]

  checker p = do
    expandedGitDir  <- expandPath p.gitDir
    expandedHomeDir <- expandPath (fromMaybe "~" p.homeDir)

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
      case remoteResult of
        Left  _ -> pure False
        Right currentUrl -> pure $ currentUrl == correctUrl

    allTrackedFilesPresent gitDir destDir branch = do
      lsResult <- gitLsTree gitDir ("origin/" <> branch)
      case lsResult of
        Left  _ -> pure False  -- remote branch not fetched yet
        Right files -> do
          fmap and $ forM files $ \f -> do
            let destPath = destDir <> "/" <> f
            fileExists destPath

  fixer p = do
    expandedGitDir  <- expandPath p.gitDir
    expandedHomeDir <- expandPath (fromMaybe "~" p.homeDir)
    changed <- liftIO $ newIORef False

    -- Step 1: init bare repo if missing
    gitDirExists <- dirExists expandedGitDir
    unless gitDirExists $ do
      args' <- mkWSCmd ["git", "init", "--bare", expandedGitDir]
      result <- cmd $ exe $ T.encodeUtf8 <$> args'
      case result of
        Right _ -> do
          putStrLn' $ "Initialized bare repo at " <> expandedGitDir
          liftIO $ writeIORef changed True
        Left err -> error $ "Failed to init bare repo: " <> show err

    -- Step 2: configure remote (add or update)
    remoteResult <- gitDirRemoteUrl expandedGitDir "origin"
    case remoteResult of
      Right currentUrl | currentUrl == p.remoteUrl ->
        pure ()  -- already correct
      Right _ -> do
        args' <- mkWSCmd ["git", "--git-dir", expandedGitDir, "remote", "set-url", "origin", p.remoteUrl]
        void $ cmd $ exe $ T.encodeUtf8 <$> args'
        liftIO $ writeIORef changed True
      Left _ -> do
        args' <- mkWSCmd ["git", "--git-dir", expandedGitDir, "remote", "add", "origin", p.remoteUrl]
        void $ cmd $ exe $ T.encodeUtf8 <$> args'
        liftIO $ writeIORef changed True

    -- Step 3: fetch (always; errors out on failure)
    fetchArgs <- mkWSCmd ["git", "--git-dir", expandedGitDir, "fetch", "origin"]
    fetchResult <- cmd $ exe $ T.encodeUtf8 <$> fetchArgs
    case fetchResult of
      Left err -> error $ "Failed to fetch from remote: " <> show err
      Right _  -> pure ()

    -- Step 4: ensure local branch exists and tracks remote
    let remoteBranch = "origin/" <> p.branch
    void $ cmd $ exe $ T.encodeUtf8 <$>
      ["git", "--git-dir", expandedGitDir, "branch", p.branch, remoteBranch]
    args' <- mkWSCmd ["git", "--git-dir", expandedGitDir, "branch", "--set-upstream-to", remoteBranch, p.branch]
    void $ cmd $ exe $ T.encodeUtf8 <$> args'

    -- Step 5: soft-checkout files missing from homeDir
    lsResult <- gitLsTree expandedGitDir ("origin/" <> p.branch)
    case lsResult of
      Left err -> error $ "Failed to list tracked files: " <> show err
      Right files -> do
        forM_ files $ \f -> do
          let destPath = expandedHomeDir <> "/" <> f
          exists <- fileExists destPath
          unless exists $ do
            putStrLn' $ "Checking out missing file: " <> f
            coArgs <- mkWSCmd ["git", "--git-dir", expandedGitDir, "--work-tree", expandedHomeDir, "checkout", "origin/" <> p.branch, "--", f]
            result <- cmd $ exe $ T.encodeUtf8 <$> coArgs
            case result of
              Right _ -> liftIO $ writeIORef changed True
              Left err -> putStrLn' $ "Warning: failed to checkout " <> f <> ": " <> tshow err

    -- Step 6: run post-change script if anything changed
    didChange <- liftIO $ readIORef changed
    when didChange $ forM_ p.runAfterChange $ \script -> do
      expandedScript <- expandPath script
      putStrLn' $ "Running post-change script: " <> expandedScript
      scriptArgs <- mkWSCmd ["bash", expandedScript]
      result <- cmd $ exe $ T.encodeUtf8 <$> scriptArgs
      case result of
        Right _ -> pure ()
        Left err -> error $ "Post-change script failed: " <> show err

  dependencies _ = return [IsProp HasGitP]

data GitTrackHomeDirP = GitTrackHomeDirP { gitDir :: Text }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GitMacOSP = GitMacOSP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GitDebianP = GitDebianP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop GitTrackHomeDirP where
  desc _ = "git track home dir"
  attrs p = Map.fromList [("gitDir", p.gitDir)]
  checker p = do
    expandedGitDir <- expandPath $ "$HOME/" <> p.gitDir
    dirExists expandedGitDir
  fixer p = do
    expandedGitDir <- expandPath $ p.gitDir
    let shellCmd = T.concat
          [ "export GIT_DIR='", expandedGitDir, "'; "
          , "("
          , "cd $HOME; "
          , "git init .; "
          , "git config --local --get-all core.bare true >/dev/null && "
          , "git config --local --replace-all core.bare false true "
          , ")"
          ]
    args' <- mkWSCmd ["bash", "-c", shellCmd]
    result <- cmd $ exe $ T.encodeUtf8 <$> args'
    case result of
      Right _ -> putStrLn' "home dir git tracking setup successfully"
      Left err -> putStrLn' $ "Failed setting up home dir git tracking: " <> tshow err

  dependencies _ = return [IsProp HasGitP]

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
      Unknown -> error "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- detectOS
    case os of
      Unknown -> error "error: Unknown OS, unable to install git"
      MacOS -> do
        args' <- mkWSCmd ["brew", "install", "git"]
        result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
      Debian -> do
        args' <- mkWSCmd ["sudo", "apt-get", "install", "-y", "git"]
        result <- cmd $ exe (T.encodeUtf8 <$> args') &> devNull
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
