{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.Git where

import WSHS.Types
import WSHS.Commands
import WSHS.Properties.MacOS
import WSHS.Properties.Debian
import Shh (exe, devNull, (&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Either (isRight)
import Data.Bool (bool)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

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
  attrs p = Map.fromList [("gitDir", gitDir p)]
  checker p = do
    expandedGitDir <- expandPath $ "$HOME/" <> gitDir p
    isRight <$> cmd (exe "test" "-d" (T.unpack expandedGitDir))
  fixer p = do
    expandedGitDir <- expandPath $ gitDir p
    let cmdtxt = [ "export GIT_DIR='"
                 , T.unpack expandedGitDir
                 , "'; "
                 , concat [ "("
                          , "cd $HOME; "
                          , "git init .; "
                          , "git config --local --get-all core.bare true >/dev/null && "
                          , "git config --local --replace-all core.bare false true "
                          , ")"
                          ]
                 ]
    result <- cmd (exe "bash" "-c" $ concat cmdtxt)
    case result of
      Right _ -> putStrLn' "home dir git tracking setup successfully"
      Left err -> putStrLn' $ "Failed setting up home dir git tracking: " <> tshow err

  dependencies _ = return [IsProp HasGitP]

instance Prop GitMacOSP where
  desc _ = "git (macOS via Homebrew)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd (exe "brew" "install" "git" &> devNull)
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = return [(IsProp HomebrewP)]

instance Prop GitDebianP where
  desc _ = "git (Debian via apt)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd (exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
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
        result <- cmd (exe "brew" "install" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
      Debian -> do
        result <- cmd (exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
