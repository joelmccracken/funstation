{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Funstation.Properties.Sudoers where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Funstation.Types
import Funstation.Commands
import Funstation.Proc
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import Shh (exe)
import qualified Data.Map.Strict as Map
import Control.Monad (unless)

-- | A drop-in file under @/etc/sudoers.d@. The main @/etc/sudoers@ is never
-- touched.
data SudoersP = SudoersP
  { filename :: Maybe Text  -- ^ Name of the drop-in, defaults to "funstation"
  , content :: Text         -- ^ Desired contents; expanded with 'expandText', so @$USER@ works
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultSudoersFilename :: Text
defaultSudoersFilename = "funstation"

sudoersDir :: Text
sudoersDir = "/etc/sudoers.d"

-- | The drop-in's full path, or an error if sudo would silently skip it.
--
-- @#includedir@ skips file names that end in @~@ or contain a @.@
-- (see @man sudoers@), so such a rule would never take effect.
sudoersPath :: MonadError WSError m => SudoersP -> m Text
sudoersPath p = do
  let name = fromMaybe defaultSudoersFilename p.filename
  case validateSudoersFilename name of
    Left err -> throwError $ WSFailure err
    Right () -> pure $ sudoersDir <> "/" <> name

validateSudoersFilename :: Text -> Either Text ()
validateSudoersFilename name
  | T.null name = Left "sudoers filename must not be empty"
  | "/" `T.isInfixOf` name = Left $ "sudoers filename must not contain '/': " <> name
  | "." `T.isInfixOf` name = Left $ "sudoers filename must not contain '.', or sudo ignores it: " <> name
  | "~" `T.isSuffixOf` name = Left $ "sudoers filename must not end in '~', or sudo ignores it: " <> name
  | otherwise = Right ()

-- | Check a candidate sudoers file's syntax with @visudo -cf@. Needs no
-- privileges, so it runs as the invoking user.
visudoCheck :: MonadIO m => FilePath -> m Bool
visudoCheck path = isRight <$> cmd (exe "visudo" "-cf" path)

instance Prop SudoersP where
  desc _ = "sudoers drop-in"
  attrs p = Map.fromList [("filename", fromMaybe defaultSudoersFilename p.filename)]
  checker p = do
    os <- asks (.os)
    case os of
      -- Sudo is configured declaratively on NixOS (security.sudo.extraRules)
      NixOS -> return True
      _ -> do
        path <- sudoersPath p
        expanded <- expandText p.content
        fileContentsCheck path expanded
  fixer p = do
    os <- asks (.os)
    case os of
      NixOS -> throwError $ WSFailure "sudoers is managed declaratively on NixOS; this property should never run its fixer here"
      _ -> do
        path <- sudoersPath p
        expanded <- expandText p.content
        let opts = FixOpts { validate = Just visudoCheck, newMode = Just "440" }
        result <- fileContentsFixWith opts path expanded
        case result of
          Nothing -> putStrLn' $ "  " <> path <> " already has correct contents"
          Just backupPath -> do
            putStrLn' $ "  Updated " <> path
            -- backups contain a '.', so sudo ignores them
            unless (backupPath == "") $
              putStrLn' $ "  (backed up original to " <> backupPath <> ")"
  dependencies _ = return []
