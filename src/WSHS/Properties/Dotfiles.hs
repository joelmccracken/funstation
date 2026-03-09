{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WSHS.Properties.Dotfiles where

import WSHS.Types
import WSHS.Commands
import Shh (exe, devNull, (&>), captureTrim, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Bool (bool)
import Data.Either (isRight)
import Control.Monad (void, forM_, forM)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

data DotfileSort
  = Symlink
  | Copy
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DotfileConfig = DotfileConfig
  { src :: Text
  , dest :: Maybe Text  -- ^ Optional destination path; if absolute, used directly
  , dot :: Bool
  , sort :: DotfileSort
  , dir :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Describes the difference between desired and current filesystem state for a dotfile
data DotfileDiff
  = DotfileCorrect              -- ^ Already in the desired state, no action needed
  | DotfileMissing              -- ^ Destination doesn't exist, needs to be created
  | DotfileBrokenSymlink        -- ^ Destination is a broken symlink, needs removal and recreation
  | DotfileWrong                -- ^ Destination exists but has wrong content/type, needs backup and recreation
  | DotfileSrcMissing Text      -- ^ Error: source file doesn't exist (carries the missing path)
  deriving (Eq, Show)

data DotfilesP = DotfilesP
  { srcDir :: Text
  , destDir :: Maybe Text  -- ^ Destination base directory, defaults to "~/" if Nothing
  , files :: [DotfileConfig]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Compute the filesystem diff for a single dotfile.
-- This determines what action (if any) is needed to bring the dotfile to the desired state.
computeDotfileDiff :: DotfileConfig -> Text -> Text -> WS DotfileDiff
computeDotfileDiff f src dest = do
  -- Check if source exists
  srcExists <- isRight <$> cmd (exe "test" "-e" (T.unpack src))
  if not srcExists
    then pure $ DotfileSrcMissing src
    else do
      -- Check if dest is a symlink (regardless of whether target exists)
      isLink <- isRight <$> cmd (exe "test" "-L" (T.unpack dest))
      -- Check if dest exists (follows symlinks, so broken symlink = False)
      destExists <- isRight <$> cmd (exe "test" "-e" (T.unpack dest))

      case (isLink, destExists) of
        (True, False) ->
          -- Symlink exists but target doesn't = broken symlink
          pure DotfileBrokenSymlink
        (_, False) ->
          -- No dest at all
          pure DotfileMissing
        _ -> do
          -- Dest exists, check if it's correct
          isCorrect <- checkSingleDotfile f src dest
          pure $ if isCorrect then DotfileCorrect else DotfileWrong


-- | Get the destination base directory, defaulting to "~/" if not set
getDestDir :: DotfilesP -> Text
getDestDir p = maybe "~/" ensureTrailingSlash p.destDir
  where
    ensureTrailingSlash t = if T.isSuffixOf "/" t then t else t <> "/"

-- | Compute the full destination path for a dotfile config.
-- If dest is set and is an absolute path (starts with /), use it directly.
-- If dest is set but relative, prepend baseDestDir (dot prefix is ignored).
-- If dest is not set, derive from src with optional dot prefix.
computeDestPath :: Text -> DotfileConfig -> Text
computeDestPath baseDestDir f =
  case f.dest of
    Just d | T.isPrefixOf "/" d -> d
    Just d -> baseDestDir <> d  -- dot prefix ignored when dest is explicit
    Nothing -> baseDestDir <> (bool "" "." f.dot) <> f.src

-- | Check if a single dotfile is in the correct state
checkSingleDotfile :: DotfileConfig -> Text -> Text -> WS Bool
checkSingleDotfile f src dest = do
  case f.sort of
    Symlink -> do
      -- Check if dest is a symlink pointing to src
      target <- cmd (exe "readlink" (T.unpack dest) |> captureTrim)
      pure $ either (const False) (\t -> TL.toStrict (TL.decodeUtf8 t) == src) target
    Copy -> do
      -- Ensure dest is not a symlink, and contents match
      isSymlink <- isRight <$> cmd (exe ["test", "-L", (T.unpack dest)])
      if isSymlink
        then pure False  -- Wrong type: should be regular file, not symlink
        else do
          diffResult <- cmd (exe "diff" "-rq" (T.unpack src) (T.unpack dest) &> devNull)
          pure $ isRight diffResult

-- | Compute expanded src and dest paths for a dotfile config
computeDotfilePaths :: DotfilesP -> DotfileConfig -> WS (Text, Text)
computeDotfilePaths p f = do
  let baseDestDir = getDestDir p
  src <- expandPath $ p.srcDir <> "/" <> f.src
  dest <- expandPath $ computeDestPath baseDestDir f
  pure (src, dest)

-- | Apply the fix for a dotfile based on its diff state.
-- Assumes the diff is not DotfileCorrect or DotfileSrcMissing (caller should handle those).
applyDotfileFix :: DotfileConfig -> Text -> Text -> DotfileDiff -> WS ()
applyDotfileFix f src dest diff = do
  -- Handle any necessary cleanup/backup based on the diff
  case diff of
    DotfileBrokenSymlink -> do
      putStrLn' $ "Removing broken symlink: " <> dest
      rmArgs <- mkWSCmd ["rm", dest]
      void $ cmd $ exe $ T.encodeUtf8 <$> rmArgs
    DotfileWrong -> do
      putStrLn' $ "Backing up existing file: " <> dest
      mvToBackup dest
    _ -> pure ()

  -- Create the dotfile (symlink or copy)
  case f.sort of
    Symlink -> do
      putStrLn' $ "Creating symlink: " <> dest <> " -> " <> src
      lnArgs <- mkWSCmd ["ln", "-s", src, dest]
      void $ cmd $ exe $ T.encodeUtf8 <$> lnArgs
    Copy -> do
      putStrLn' $ "Copying: " <> src <> " -> " <> dest
      cpArgs <- mkWSCmd ["cp", "-r", src, dest]
      void $ cmd $ exe $ T.encodeUtf8 <$> cpArgs

instance Prop DotfilesP where
  desc _ = "dotfiles management"
  attrs _ = mempty
  checker p = do
    results <- forM p.files $ \f -> do
      (src, dest) <- computeDotfilePaths p f
      diff <- computeDotfileDiff f src dest
      case diff of
        DotfileSrcMissing path -> error $ "Source file does not exist: " <> T.unpack path
        DotfileCorrect -> pure True
        _ -> pure False
    return $ all id results

  fixer p = do
    forM_ p.files $ \f -> do
      (src, dest) <- computeDotfilePaths p f
      diff <- computeDotfileDiff f src dest
      case diff of
        DotfileSrcMissing path -> error $ "Source file does not exist: " <> T.unpack path
        DotfileCorrect -> pure ()
        _ -> applyDotfileFix f src dest diff

  dependencies _ = return []
