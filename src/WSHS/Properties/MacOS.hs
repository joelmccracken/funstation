{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.MacOS where

import WSHS.Types
import WSHS.Commands
import Shh (exe, devNull, (&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Either (isRight)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map

data XCodeCLIToolsP = XCodeCLIToolsP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HomebrewP = HomebrewP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop XCodeCLIToolsP where
  desc _ = "Xcode CLI Tools"
  attrs _ = mempty
  checker _ = isRight <$> cmd (exe "pkgutil" "--pkg-info=com.apple.pkg.CLTools_Executables" &> devNull)
  fixer _ = do
      putStrLn' "Installing Xcode CLI Tools..."

      -- Create marker file that triggers CLT in softwareupdate list
      void $ cmd (exe "touch" "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

      -- Find and install the CLT package
      let findAndInstall = "softwareupdate -i \"$(softwareupdate -l 2>&1 | grep -o 'Command Line Tools for Xcode-[0-9.]*' | head -1)\""
      result <- cmd (exe "bash" "-c" findAndInstall)

      -- Clean up marker
      -- TODO bracket to clean up
      void $ cmd (exe "rm" "-f" "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

      case result of
        Left err -> error $ "Failed to install Xcode CLI tools: " <> show err
        Right _ -> putStrLn' "Xcode CLI tools installed successfully"

  dependencies _ = return []

instance Prop HomebrewP where
  desc _ = "homebrew package manager for macOS"
  attrs _ = mempty
  checker _ = hasCmd' "brew"
  fixer _ = do
    result <- cmd (exe "bash" "-c" "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" &> devNull)
    case result of
      Right _ -> putStrLn' "Homebrew installed successfully"
      Left err -> putStrLn' $ "Failed to install homebrew: " <> tshow err
  dependencies _ = return [(IsProp XCodeCLIToolsP)]

data HomebrewBundleP = HomebrewBundleP
  { brewfile :: Text   -- ^ path to the Brewfile
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HomebrewBundleP where
  desc _ = "homebrew bundle"
  attrs p = Map.fromList [("brewfile", p.brewfile)]
  checker p = do
    brewInstalled <- hasCmd' "brew"
    if not brewInstalled
      then return False
      else isRight <$> cmd (exe "brew" "bundle" "check" "--no-upgrade" ("--file=" <> T.unpack p.brewfile) &> devNull)
  fixer p = do
    result <- cmd (exe "brew" "bundle" "install" ("--file=" <> T.unpack p.brewfile))
    case result of
      Right _ -> putStrLn' "Homebrew bundle installed."
      Left err -> error $ "brew bundle install failed: " <> show err
  dependencies _ = return [IsProp HomebrewP]
