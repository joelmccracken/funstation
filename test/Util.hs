module Util where

import Test.Hspec
import Data.Text qualified as T

import Funstation.Types
import Funstation.Properties.GitHomeDir (GitHomeDirP (..))



failLeft result =
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Right a -> pure a

-- | Run a monadic action and assert its result equals the expected value.
shouldBeM :: (Show a, Eq a) => a -> IO a -> Expectation
shouldBeM expected op = do
  res <- op
  res `shouldBe` expected
