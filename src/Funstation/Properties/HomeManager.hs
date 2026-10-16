{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.HomeManager where

import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Shh (exe, captureTrim, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (eitherDecode, FromJSON)
import Data.Either (isRight)
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

getWorkstationNameRaw :: (MonadReader Settings m, MonadError WSError m) => m Text
getWorkstationNameRaw = do
  settings <- ask
  pure (unWorkstationName settings.workstation)

-- | Shell prelude that loads Nix (and the user/home-manager profiles)
nixProfileSource :: Text
nixProfileSource = ". /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh >/dev/null 2>&1; "

instance Prop HomeManagerP where
  desc _ = "home-manager configuration"
  attrs p = Map.fromList [("dir", p.dir)]

  checker p = do
    hmInstalled <- isRight <$> cmd (exe "bash" "-c" (T.unpack (nixProfileSource <> "command -v home-manager")) |> captureTrim)
    if not hmInstalled
      then return False
      else do
        ws <- getWorkstationNameRaw
        flakeOut <- liftIO $ mkFlakeOut ws
        expandedDir <- expandPath p.dir
        let buildCmd = nixProfileSource
                    <> "cd " <> expandedDir
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
    ws <- getWorkstationNameRaw
    flakeOut <- liftIO $ mkFlakeOut ws
    expandedDir <- expandPath p.dir
    -- Source the Nix profile so nix is on PATH.
    let runCmd' = nixProfileSource
              <> "cd " <> expandedDir
              <> " && nix -v -L --show-trace run "
              <> flakeOut
    orFail "Home Manager activation failed" $ runCmd ["bash", "-c", runCmd'] id
    putStrLn' "Home Manager configuration activated."

  dependencies _ = return []
