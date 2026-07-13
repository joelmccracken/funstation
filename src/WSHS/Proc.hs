{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}

module WSHS.Proc where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, asks)
import System.IO (hFlush, stdout)
import Shh (exe, devNull, (&>), Failure, Proc, tryFailure)
import GHC.Stack (withFrozenCallStack)
import Control.Monad.Except (MonadError, throwError)
import WSHS.Types
import WSHS.Sudo


-- | Run a privilege-escalating command in the WS monad.
-- Reads the sudo command from Settings; uses AccessMode to choose the
-- permission check (read or write).
privCmd :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => AccessMode -> Text -> [Text] -> m (Either Failure ())
privCmd mode pth args = do
  sc <- asks (.sudoCmd)

  c  <- liftIO $ mkPrivCmd sc mode pth args
  runCmd c id

mkWSCmd :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => [Text] -> m [Text]
mkWSCmd c = do
  inter <- asks (.opts.interactive)
  v     <- asks (.opts.verbose)
  when (v || inter) $ liftIO $ TIO.putStrLn $ "running: " <> T.intercalate " " c
  when inter $ do
    response <-
      liftIO $ do
        TIO.putStr "Continue? [c/q]: "
        hFlush stdout
        liftIO getLine
    case response of
      "q" -> throwError WSAborted
      _   -> pure ()
  pure c

runCmd
  :: (MonadIO m, MonadReader Settings m, MonadError WSError m)
  => [Text]
  -> (Proc () -> Proc b)
  -> m (Either Failure b)
runCmd cs modifier = do
  -- undefined
  args' <- mkWSCmd cs
  cmd $ (modifier $ exe (T.encodeUtf8 <$> args'))

-- | Create a directory (and any missing parents).
mkDir' :: (MonadIO m, MonadReader Settings m, MonadError WSError m) => Text -> m ()
mkDir' path = do
  void $ runCmd ["mkdir", "-p", path] (&> devNull)

cmd :: MonadIO m => Proc a -> m (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c
