{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.HomeManager where

import WSHS.Types
import WSHS.Commands
import Shh (exe, captureTrim, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (eitherDecode, FromJSON)
import Data.Either (isRight)
import Control.Monad.Reader (ask)
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

getWorkstation :: WS Text
getWorkstation = do
  settings <- ask
  case settings.opts.command of
    Bootstrap { workstation = ws } -> return ws
    _ -> error "HomeManagerP can only be used from a Bootstrap command"

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
        let buildCmd = "cd " <> T.unpack expandedDir
                    <> " && nix build --json --dry-run -v -L "
                    <> T.unpack flakeOut
                    <> " --show-trace"
        result <- cmd (exe "bash" "-c" buildCmd |> captureTrim)
        case result of
          Left _ -> return False
          Right jsonBytes ->
            case eitherDecode jsonBytes :: Either String [NixBuildEntry] of
              Left _ -> return False
              Right [] -> return False
              Right (entry:_) ->
                let outPath = T.unpack entry.outputs.out
                in isRight <$> cmd (exe "test" "-e" outPath)

  fixer p = do
    ws <- getWorkstation
    flakeOut <- liftIO $ mkFlakeOut ws
    expandedDir <- expandPath p.dir
    let runCmd = "cd " <> T.unpack expandedDir
              <> " && nix -v -L --show-trace run "
              <> T.unpack flakeOut
    result <- cmd (exe "bash" "-c" runCmd)
    case result of
      Right _ -> putStrLn' "Home Manager configuration activated."
      Left err -> error $ "Home Manager activation failed: " <> show err

  dependencies _ = return []
