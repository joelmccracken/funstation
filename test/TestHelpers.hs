module TestHelpers where

import Test.Hspec
import Data.Text qualified as T
import System.Posix.Files (setFileMode)
import Control.Exception (bracket_)

import Funstation.Types
import Funstation.Properties.GitHomeDir (GitHomeDirP (..))



-- | Temporarily set a file's mode, restoring the original after the action.
-- Ensures cleanup can proceed even if the action throws.
withFileMode :: FilePath -> Int -> IO a -> IO a
withFileMode path newMode action =
  bracket_
    (setFileMode path (fromIntegral newMode))
    (setFileMode path 0o644)
    action

failLeft result =
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Right a -> pure a

-- | Run a monadic action and assert its result equals the expected value.
shouldBeM :: (Show a, Eq a) => a -> IO a -> Expectation
shouldBeM expected op = do
  res <- op
  res `shouldBe` expected
