{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}

module WSHS.Properties.Basic where

import WSHS.Types
import WSHS.Commands
import WSHS.Properties.Git ()
import Shh (exe)
import Data.Text qualified as T
import Data.Either (isRight)
import Data.List (intercalate)
import Control.Monad.Reader (asks)

instance Prop BasicSetupP where
  desc _ = "basic setup"
  attrs _ = mempty
  checker _ = return True -- dummy prop, wrapper for dependencies
  fixer _ = return () -- all action in dependencies
  dependencies _ = return [(IsProp HasGitP)]

instance Prop WSConfigDirP where
  desc _ = "wshs configuration directory"
  attrs _ = mempty
  checker _ = do
    cfg <- asks configuration
    isRight <$> cmd (exe "bash" "-c" $ concat ["test -d ", T.unpack $ cfg.configDir])
  fixer _ = do
    cfg <- asks configuration
    let cloneCmd =
          intercalate " "
            [ "git clone"
            , "--branch"
            , T.unpack cfg.configRepoBranch
            , T.unpack cfg.configRepoUrl
            , T.unpack cfg.configDir
            ]
    result <- cmd (exe "bash" "-c" $ cloneCmd)
    case result of
      Right _ -> putStrLn' $ "Cloned repository to " <> cfg.configDir
      Left err -> putStrLn' $ "Failed to clone repository: " <> tshow err
  dependencies _ = return [IsProp HasGitP]
