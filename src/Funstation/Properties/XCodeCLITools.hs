{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.XCodeCLITools where

import Control.Monad.Except (throwError)
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Shh (exe, devNull, (&>))
import Data.Either (isRight)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data XCodeCLIToolsP = XCodeCLIToolsP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop XCodeCLIToolsP where
  desc _ = "Xcode CLI Tools"
  attrs _ = mempty
  checker _ = isRight <$> cmd (exe "pkgutil" "--pkg-info=com.apple.pkg.CLTools_Executables" &> devNull)
  fixer _ = do
      putStrLn' "Installing Xcode CLI Tools..."

      -- Create marker file that triggers CLT in softwareupdate list
      void $ runCmd ["touch", "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"] id

      -- Find and install the CLT package
      let findAndInstall = "softwareupdate -i \"$(softwareupdate -l 2>&1 | grep -o 'Command Line Tools for Xcode-[0-9.]*' | head -1)\""
      result <- runCmd ["bash", "-c", findAndInstall] id

      -- Clean up marker
      -- TODO bracket to clean up
      void $ runCmd ["rm", "-f", "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"] id

      case result of
        Left err -> throwError $ WSFailure $ "Failed to install Xcode CLI tools: " <> tshow err
        Right _ -> putStrLn' "Xcode CLI tools installed successfully"

  dependencies _ = return []
