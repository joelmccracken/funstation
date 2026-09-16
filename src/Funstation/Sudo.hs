{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Funstation.Sudo (module Funstation.Sudo) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Either (isRight)
import Shh (exe, tryFailure, (<<<))
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.FilePath (takeDirectory)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Monad (forever, void)

-- | Check if sudo is needed to read a path.
-- For existing files, checks read permission on the file.
-- For non-existent files, returns False (nothing to read).
needsSudoRead :: Text -> IO Bool
needsSudoRead path = do
  let pathStr = T.unpack path
  exists <- isRight <$> tryFailure (exe "test" "-e" pathStr)
  if exists
    then do
      readable <- isRight <$> tryFailure (exe "test" "-r" pathStr)
      pure $ not readable
    else
      pure False

-- | The nearest ancestor of a path that exists, including the path itself.
--
-- Walks up until something exists. Terminates at the filesystem root (and at
-- @.@ for relative paths), where 'takeDirectory' is a fixed point.
nearestExistingAncestor :: FilePath -> IO FilePath
nearestExistingAncestor path = do
  exists <- isRight <$> tryFailure (exe "test" "-e" path)
  if exists
    then pure path
    else do
      let parentDir = takeDirectory path
      if parentDir == path
        then pure path
        else nearestExistingAncestor parentDir

-- | Check if sudo is needed to write to a path.
--
-- Checks write permission on path. For existing file, use the file itself.
-- For a missing file, this is the nearest existing parent.
needsSudo :: Text -> IO Bool
needsSudo path = do
  target <- nearestExistingAncestor (T.unpack path)
  writable <- isRight <$> tryFailure (exe "test" "-w" target)
  pure $ not writable

-- | Which filesystem permission to check when deciding whether sudo is needed.
data AccessMode = ReadAccess | WriteAccess

needsSudoFor :: AccessMode -> Text -> IO Bool
needsSudoFor ReadAccess  = needsSudoRead
needsSudoFor WriteAccess = needsSudo

-- | Build an IO Cmd that prepends the given sudo command if the path requires
-- the specified access, or @env@ (a no-op prefix) otherwise.
-- The returned IO Cmd can be chained with shh operators like |> and &>.
mkPrivCmd :: String -> AccessMode -> Text -> [Text] -> IO [Text]
mkPrivCmd sudoCmd mode pth args = do
  useSudo <- needsSudoFor mode pth
  pure $ if useSudo
    then ((T.pack sudoCmd) : args)
    else ("env"   : args)

-- | Refresh sudo credentials by running @sudo -v@.
-- Reads password from file if provided, otherwise prompts interactively.
-- Returns True if successful, False otherwise.
refreshSudo :: Maybe Text -> IO Bool
refreshSudo Nothing = isRight <$> tryFailure (exe "sudo" "-v")
refreshSudo (Just passFile) = do
  passContent <- TIO.readFile (T.unpack passFile)
  let pass = case T.lines passContent of
        []    -> error $ "sudo-pass-file is empty: " <> T.unpack passFile
        (l:_) -> T.strip l
  isRight <$> tryFailure (exe "sudo" "-S" "-p" "" "-v" <<< LBS.pack (T.unpack pass <> "\n"))

-- | Start a background thread that refreshes sudo credentials every 60 seconds.
-- Returns the ThreadId so it can be killed when done.
startSudoRefreshLoop :: IO ThreadId
startSudoRefreshLoop = forkIO $ forever $ do
  threadDelay (60 * 1000000)
  void $ refreshSudo Nothing

-- | Initialize sudo credential caching.
-- Read from provided password file, or prompt for password interactively. 
initSudoCache :: Maybe Text -> IO (Maybe ThreadId)
initSudoCache mpassFile = do
  case mpassFile of
    Nothing -> putStrLn "Initializing sudo credential cache (you may be prompted for your password)..."
    Just _  -> putStrLn "Initializing sudo credential cache from password file..."
  success <- refreshSudo mpassFile
  if success
    then do
      putStrLn "Sudo credentials cached. Starting background refresh..."
      tid <- startSudoRefreshLoop
      pure (Just tid)
    else do
      putStrLn "Warning: Failed to cache sudo credentials. Continuing without --sudo-cache mode."
      pure Nothing
