{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module Funstation.Properties.ShellCommand where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Types (FromJSON, ToJSON, parseJSON, toJSON, withObject, object, (.:), (.:?), (.!=), (.=))
import Data.Either (isRight)
import Data.Text (Text)
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import GHC.Generics (Generic)
import Shh (Failure)
import qualified Data.Map.Strict as Map

-- | An arbitrary pair of shell commands: @checker@ decides whether the
-- property is satisfied (exit 0 means yes), and @fixer@ runs when it
-- is not.
data ShellCommandP = ShellCommandP
  { label      :: Maybe Text
    -- ^ Human-readable name for the property, shown while it runs
  , checkerCmd :: Text
    -- ^ Satisfied when this exits 0
  , fixerCmd   :: Text
    -- ^ Run when the checker exits non-zero
  , verify     :: Bool
    -- ^ Re-run the checker after the fixer to confirm it took (default 'True')
  } deriving (Eq, Show, Generic)

instance FromJSON ShellCommandP where
  parseJSON = withObject "ShellCommandP" $ \o ->
    ShellCommandP
      <$> o .:? "desc"
      <*> o .:  "checker"
      <*> o .:  "fixer"
      <*> o .:? "verify" .!= True

instance ToJSON ShellCommandP where
  toJSON p = object
    [ "desc"    .= p.label
    , "checker" .= p.checkerCmd
    , "fixer"   .= p.fixerCmd
    , "verify"  .= p.verify
    ]

-- | Run a script with @bash -c@, yielding its exit status.
--
-- The script is passed to bash as written, so normal shell-expansion
-- bashisms will work.
runShell :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m (Either Failure ())
runShell script = runCmd ["bash", "-c", script] id

instance Prop ShellCommandP where
  desc p = "shell command" <> maybe "" (": " <>) p.label
  attrs p = Map.fromList [("checker", p.checkerCmd), ("fixer", p.fixerCmd)]
  checker p = isRight <$> runShell p.checkerCmd
  fixer p = do
    result <- runShell p.fixerCmd
    case result of
      Left err -> throwError $ WSFailure $ "shell command fixer failed: " <> tshow err
      Right () -> when p.verify $ do
        ok <- isRight <$> runShell p.checkerCmd
        unless ok $ throwError $ WSFailure $
          "fixer for " <> desc p <> " exited 0 but its checker still fails"
  dependencies _ = return []
