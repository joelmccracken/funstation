{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.HomebrewBundle where

import Control.Monad.Reader (asks)
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import Funstation.Properties.Homebrew
import Shh (exe, devNull, (&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Either (isRight)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map

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
      else do
        expandedBrewfile <- expandPath p.brewfile
        isRight <$> cmd (exe "brew" "bundle" "check" "--no-upgrade" ("--file=" <> T.unpack expandedBrewfile) &> devNull)
  fixer p = do
    expandedBrewfile <- expandPath p.brewfile
    v <- asks (.opts.verbose)
    let verboseFlag = ["--verbose" | v]
    orFail "brew bundle install failed" $
      runCmd (["brew", "bundle", "install"] <> verboseFlag <> ["--file=" <> expandedBrewfile]) id
    putStrLn' "Homebrew bundle installed."
  dependencies _ = return [IsProp HomebrewP]
