{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.HomeManager where

import Control.Monad.Except (throwError)
import WSHS.Types
import WSHS.Commands
import WSHS.Proc
import Shh (exe, captureTrim, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Aeson (eitherDecode, FromJSON)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)
import qualified Data.Map.Strict as Map

data HomeManagerP = HomeManagerP
  { dir :: Text   -- ^ path to the directory containing the home-manager flake
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- JSON types for parsing `nix build --json` output
data NixBuildOutputs = NixBuildOutputs
  { out :: Text
  } deriving (Generic, FromJSON)

data NixBuildEntry = NixBuildEntry
  { outputs :: NixBuildOutputs
  } deriving (Generic, FromJSON)

mkFlakeOut :: Text -> IO Text
mkFlakeOut workstation = do
  username <- T.pack <$> getEnv "USER"
  pure $ ".#homeConfigurations.\"" <> username <> "@" <> workstation <> "\".activationPackage"

getWorkstation :: (MonadReader Settings m, MonadError WSError m) => m Text
getWorkstation = do
  settings <- ask
  pure settings.opts.workstation

instance Prop HomeManagerP where
  desc _ = "home-manager configuration"
  attrs p = Map.fromList [("dir", p.dir)]

  checker p = do
    hmInstalled <- hasCmd' "home-manager"
    if not hmInstalled
      then return False
      else do
        ws <- getWorkstation
        flakeOut <- liftIO $ mkFlakeOut ws
        expandedDir <- expandPath p.dir
        let buildCmd = "cd " <> expandedDir
                    <> " && nix build --json --dry-run -v -L "
                    <> flakeOut
                    <> " --show-trace"
        result <- cmd (exe "bash" "-c" (T.unpack buildCmd) |> captureTrim)
        case result of
          Left _ -> return False
          Right jsonBytes ->
            case eitherDecode jsonBytes :: Either String [NixBuildEntry] of
              Left _ -> return False
              Right [] -> return False
              Right (entry:_) ->
                let outPath = T.unpack entry.outputs.out
                in fileExists (T.pack outPath)

  fixer p = do
    ws <- getWorkstation
    flakeOut <- liftIO $ mkFlakeOut ws
    expandedDir <- expandPath p.dir
    let runCmd' = "cd " <> expandedDir
              <> " && nix -v -L --show-trace run "
              <> flakeOut
    result <- runCmd ["bash", "-c", runCmd'] id
    either
      (\err-> throwError $ WSFailure $ "Home Manager activation failed: " <> tshow err)
      (const $ putStrLn' "Home Manager configuration activated.")
      result

  dependencies _ = return []
