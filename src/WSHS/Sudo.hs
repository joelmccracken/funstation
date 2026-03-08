{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module WSHS.Sudo (module WSHS.Sudo) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Either (isRight)
import Shh (exe, Cmd, tryFailure, (<<<))
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

-- | Check if sudo is needed to write to a path.
-- For existing files, checks write permission on the file.
-- For non-existent files, checks write permission on the parent directory.
needsSudo :: Text -> IO Bool
needsSudo path = do
  let pathStr = T.unpack path
  exists <- isRight <$> tryFailure (exe "test" "-e" pathStr)
  if exists
    then do
      writable <- isRight <$> tryFailure (exe "test" "-w" pathStr)
      pure $ not writable
    else do
      let parentDir = takeDirectory pathStr
      writable <- isRight <$> tryFailure (exe "test" "-w" parentDir)
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
-- If a password file path is provided, reads the password from that file
-- and pipes it to @sudo -S -v@. Otherwise, prompts interactively.
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
-- If a password file path is provided, reads password from it.
-- Otherwise, prompts for password interactively.
-- Returns the ThreadId of the refresh loop, or Nothing if initial auth failed.
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
