module Util where

import Test.Hspec
import Data.Text qualified as T

import WSHS.Types
import WSHS.Properties.GitHomeDir (GitHomeDirP (..))



failLeft result =
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Right a -> pure a
