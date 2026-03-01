{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module WSHS.Properties.MacOS where

import WSHS.Types
import WSHS.Commands
import Shh (exe, devNull, (&>))
import Data.Either (isRight)
import Control.Monad (void)

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
