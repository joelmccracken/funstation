{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module WSHS (module WSHS) where

import Options.Applicative
import Options.Applicative qualified as App
-- import Shelly qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
-- import Path qualified
-- import Path ((</>))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Either (isRight)
import Data.Maybe (isJust, fromMaybe)
import GHC.Stack
import Data.List (intercalate)
import Shh (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure, (<<<), Cmd)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Yaml (decodeFileThrow)
-- import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Bool (bool)
import Control.Monad (void, forM_, forM, unless, forever, when)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Monad.IO.Class
--import WSHS.Types
-- import WSHS.Types.Configuration
import Control.Monad.State
import Control.Monad.Reader
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import WSHS.Commands qualified as Cmd
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Aeson.Types hiding (Parser, Options)
-- import Data.Aeson.TH hiding (Options)
import GHC.Generics (Generic)
import System.FilePath (takeDirectory)

-- have a config.yaml in a known location
-- have a current.yaml at a known location for the "right now" settings
-- current workstation name, etc

data WSState =
  WSState
  { props :: Set IsProp
  }

type WSStack a = ReaderT Settings (StateT WSState IO) a

data Settings = Settings
  { opts :: Options
  , configuration :: Configuration
  }

data Property
  = GitHomeDir GitTrackHomeDirP
  | Dotfiles DotfilesP
  | NixDaemon NixDaemonP
  deriving (Show, Generic)

instance ToJSON Property where
  toEncoding = genericToEncoding defaultOptions { sumEncoding =
                                                    TaggedObject
                                                    { tagFieldName = "type"
                                                    , contentsFieldName = "params"
                                                    }
                                                }

instance FromJSON Property where
  parseJSON = genericParseJSON defaultOptions { sumEncoding =
                                                    TaggedObject
                                                    { tagFieldName = "type"
                                                    , contentsFieldName = "params"
                                                    }
                                                }

data Configuration = Configuration
  { configDir :: Text
  , configRepoUrl :: Text
  , configRepoOrigin :: Text
  , configRepoBranch :: Text
  , properties :: [Property]
  }
  deriving (Generic, Show, FromJSON)

data NixSubcommand
  = NixRestart
  deriving (Show)

data Command
  = Bootstrap
    { configPath :: FilePath
    , workstation :: Text
    }
  | Nix NixSubcommand
  deriving (Show)

data Options = Options
  { command :: Command
  , sudoCache :: Bool  -- ^ If True, cache sudo credentials and refresh in background
  , sudoPassFile :: Maybe Text  -- ^ Optional path to file containing sudo password
  }
  deriving (Show)

newtype WS a
  = WS
  { unWS :: WSStack a
  }
  deriving newtype
  ( Monad
  , MonadReader Settings
  , MonadState WSState
  , Applicative
  , Functor
  , MonadIO
  )

data OS = MacOS | Debian | Unknown
  deriving (Show, Eq)

putStrLn' :: Text -> WS ()
putStrLn' t = liftIO $ putStrLn $ T.unpack t

tshow :: Show s => s -> Text
tshow = T.pack . show

cmd :: MonadIO m => Proc a -> m (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c

detectOS :: MonadIO m => m OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> captureTrim)
  case osCheck of
    Left e -> error $ "error detecting OS: " <> show e
    Right "Darwin" -> return MacOS
    Right "Linux" -> detectLinuxOS
    Right _ -> return Unknown

detectLinuxOS :: MonadIO m => m OS
detectLinuxOS = do
  -- Check if it's Debian-based
  debianCheck <- cmd (exe "test" "-f" "/etc/debian_version" &> devNull)
  case debianCheck of
    Right _ -> return Debian
    Left _ -> return Unknown

which :: Text -> WS (Maybe Text)
which cmdName = do
  result <- cmd (exe "which" (T.unpack cmdName) |> captureTrim)
  pure $ either (const Nothing) (Just . TL.toStrict . TL.decodeUtf8 ) result

hasCmd' :: Text -> WS Bool
hasCmd' cmdName = isJust <$> which cmdName

-- | Expand a path using bash filename expansion (resolves ~, $HOME, etc.)
expandPath :: MonadIO m => Text -> m Text
expandPath path = do
  result <- cmd (exe "bash" "-c" ("echo " <> T.unpack path) |> captureTrim)
  pure $ either (const path) (TL.toStrict . TL.decodeUtf8) result

-- | Check if sudo is needed to read a path.
-- For existing files, checks read permission on the file.
-- For non-existent files, returns False (nothing to read).
needsSudoRead :: Text -> IO Bool
needsSudoRead path = do
  let pathStr = T.unpack path
  exists <- isRight <$> cmd (exe "test" "-e" pathStr)
  if exists
    then do
      readable <- isRight <$> cmd (exe "test" "-r" pathStr)
      pure $ not readable
    else
      pure False

-- | Check if sudo is needed to write to a path.
-- For existing files, checks write permission on the file.
-- For non-existent files, checks write permission on the parent directory.
needsSudo :: Text -> IO Bool
needsSudo path = do
  let pathStr = T.unpack path
  exists <- isRight <$> cmd (exe "test" "-e" pathStr)
  if exists
    then do
      writable <- isRight <$> cmd (exe "test" "-w" pathStr)
      pure $ not writable
    else do
      let parentDir = takeDirectory pathStr
      writable <- isRight <$> cmd (exe "test" "-w" parentDir)
      pure $ not writable


-- | Ensure a parent directory exists, using sudo only if needed.
ensureParentDir :: Text -> WS ()
ensureParentDir path = do
  let parentDir = T.pack $ takeDirectory (T.unpack path)
  exists <- isRight <$> cmd (exe "test" "-d" (T.unpack parentDir))
  unless exists $ do
    result <- maybeSudo parentDir ["mkdir", "-p", T.unpack parentDir]
    case result of
      Right _ -> pure ()
      Left err -> error $ "Failed to create directory " <> T.unpack parentDir <> ": " <> show err

-- | Get "owner:group" for a path, for use with chown.
-- Uses ls -ld which works on both macOS and Linux.
getOwnerGroup :: Text -> WS Text
getOwnerGroup path = do
  result <- cmd (exe "ls" "-ld" (T.unpack path) |> captureTrim)
  case result of
    Left _ -> pure "root:root"
    Right bytes ->
      case words $ TL.unpack $ TL.decodeUtf8 bytes of
        (_:_:owner:group:_) -> pure $ T.pack owner <> ":" <> T.pack group
        _ -> pure "root:root"

mvToBackup :: Text -> WS ()
mvToBackup path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- cmd (exe "mv" (T.unpack path) (T.unpack backupPath))
  case result of
    Right _ -> putStrLn' $ "Moved " <> path <> " to " <> backupPath
    Left err -> error $ "Failed to move file: " <> show err


-- | Core helper: run args with the given sudo command if the needs check passes,
-- or via env otherwise. Pass "sudo" normally; inject a fake command in tests.
maybeSudoWithCmd :: String -> (Text -> IO Bool) -> Text -> [String] -> WS (Either Failure ())
maybeSudoWithCmd sudoCmd needsFn pth args = do
  useSudo <- liftIO $ needsFn pth
  if useSudo
    then cmd (exe sudoCmd args)
    else cmd (exe "env" args)

-- | Run a command with sudo if the path requires write access, or via env otherwise.
-- Using env as a no-op prefix keeps both branches structurally identical.
maybeSudo :: Text -> [String] -> WS (Either Failure ())
maybeSudo = maybeSudoWithCmd "sudo" needsSudo

-- | Like maybeSudo but checks read access instead of write access.
maybeSudoRead :: Text -> [String] -> WS (Either Failure ())
maybeSudoRead = maybeSudoWithCmd "sudo" needsSudoRead


-- | Core helper for the Proc-returning variants, allowing sudo command injection.
-- Returns an IO Cmd so callers can chain shh operators like |> and &>.
maybeSudoWithCmd' :: LBS.ByteString -> (Text -> IO Bool) -> Text -> [LBS.ByteString] -> IO Cmd
maybeSudoWithCmd' sudoCmd needsFn pth args = do
  useSudo <- needsFn pth
  if useSudo
    then pure (exe $ sudoCmd : args)
    else pure (exe $ "env" : args)

maybeSudo' :: Text -> [LBS.ByteString] -> IO Cmd
maybeSudo' = maybeSudoWithCmd' "sudo" needsSudo

maybeSudoRead' :: Text -> [LBS.ByteString] -> IO Cmd
maybeSudoRead' = maybeSudoWithCmd' "sudo" needsSudoRead



-- | Move a file to a timestamped backup, using sudo only if needed.
mvToBackupAuto :: Text -> WS Text
mvToBackupAuto path = do
  timestamp <- liftIO $ round <$> getPOSIXTime
  let backupPath = path <> "." <> T.pack (show (timestamp :: Integer))
  result <- maybeSudo path ["mv", T.unpack path, T.unpack backupPath]
  case result of
    Right _ -> do
      putStrLn' $ "  Backed up " <> path <> " to " <> backupPath
      pure backupPath
    Left err -> error $ "Failed to backup file: " <> show err

{-# DEPRECATED mvToBackupSudo "Use mvToBackupAuto instead" #-}
mvToBackupSudo :: Text -> WS Text
mvToBackupSudo = mvToBackupAuto

-- | Check if a file has the desired contents.
-- Returns True if the file exists and matches, False otherwise.
fileContentsCheck :: Text -> Text -> WS Bool
fileContentsCheck path content = do
  -- Create temp file with desired content
  tempFileResult <- cmd (exe "mktemp" |> captureTrim)
  case tempFileResult of
    Left err -> error $ "Failed to create temp file: " <> show err
    Right tempFileBytes -> do
      let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

      -- Write desired content to temp file
      liftIO $ TIO.writeFile tempFile content

      -- Check if target file exists
      targetExists <- isRight <$> cmd (exe "test" "-e" (T.unpack path))

      result <- if not targetExists
        then pure False  -- Target doesn't exist, needs fixing
        else do
          -- Compare using diff (only need read access to target file)

          diffCmd <- liftIO $ maybeSudoRead' path ["diff", "-q", LBS.pack tempFile, LBS.pack (T.unpack path)]
          diffResult <- cmd $ diffCmd &> devNull

          -- useSudo <- liftIO $ needsSudoRead path
          -- diffResult <- if useSudo
          --   then cmd (exe "sudo" "diff" "-q" tempFile (T.unpack path) &> devNull)
          --   else cmd (exe "env" "diff" "-q" tempFile (T.unpack path) &> devNull)
          pure $ isRight diffResult

      -- Clean up temp file
      -- TODO bracket to clean up
      void $ cmd (exe "rm" "-f" tempFile)

      pure result

-- | Ensure a file has the desired contents.
-- Returns Nothing if no change was needed, Just backupPath if the file was updated.
-- The backupPath will be empty string if no backup was needed (file didn't exist).
-- TODO think about what parts to reuse/share (e.g. with dotfiles code)
fileContentsFix :: Text -> Text -> WS (Maybe Text)
fileContentsFix path content = do
  -- First check if file already has correct contents
  isCorrect <- fileContentsCheck path content
  if isCorrect
    then pure Nothing  -- No change needed
    else do
      -- Create temp file with desired content
      tempFileResult <- cmd (exe "mktemp" |> captureTrim)
      case tempFileResult of
        Left err -> error $ "Failed to create temp file: " <> show err
        Right tempFileBytes -> do
          let tempFile = TL.unpack $ TL.decodeUtf8 tempFileBytes

          -- Write desired content to temp file
          liftIO $ TIO.writeFile tempFile content

          -- Check if target exists; capture owner info before any changes
          targetExists <- isRight <$> cmd (exe "test" "-e" (T.unpack path))
          ownerGroup <- if targetExists
            then getOwnerGroup path
            else getOwnerGroup (T.pack $ takeDirectory (T.unpack path))

          -- Back up existing file if present
          backupPath <- if targetExists
            then mvToBackupAuto path
            else pure ""

          -- Move temp file to target location (only use sudo if needed)
          moveResult <- maybeSudo path ["mv", tempFile, T.unpack path]
          case moveResult of
            Left err -> error $ "Failed to move file to " <> T.unpack path <> ": " <> show err
            Right _ -> pure ()

          -- Restore original ownership when sudo was used
          void $ maybeSudo path ["chown", T.unpack ownerGroup, T.unpack path]
          pure $ Just backupPath

class Prop p where
  desc :: p -> Text
  attrs :: p -> Map.Map Text Text
  -- checker: return true if property already fulfilled, false if fixer is required
  checker :: p -> WS Bool
  fixer :: p -> WS ()
  dependencies :: p -> WS [IsProp]

data IsProp where
  IsProp :: Prop p => p -> IsProp

instance Prop IsProp where
  desc (IsProp p) =  desc p
  attrs (IsProp p) =  attrs p
  checker (IsProp p) =  checker p
  fixer (IsProp p) =  fixer p
  dependencies (IsProp p) =  dependencies p

instance Eq IsProp where
  (IsProp a) == (IsProp b) = do
    desc a == desc b && attrs a == attrs b

instance Ord IsProp  where
  (IsProp a) `compare` (IsProp b) =
    case desc a `compare` desc b of
      EQ ->  attrs a `compare` attrs b
      ord -> ord

-- unIsProp :: IsProp -> (forall p. IsProp p => p -> a) -> a
-- unIsProp (IsProp p) f = f p

-- data ExampleP = ExampleP
--   deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- instance Prop ExampleP where
--   desc _ = undefined
--   attrs _ = undefined
--   checker _ = undefined
--   fixer _ = undefined
--   dependencies _ = undefined


data GitTrackHomeDirP = GitTrackHomeDirP { gitDir :: Text }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitTrackHomeDirP where
  desc _ = "git track home dir"
  attrs p = Map.fromList [("gitDir", gitDir p)]
  checker p =
    isRight <$> cmd (exe "bash" "-c" $ concat ["test -d $HOME/", T.unpack $ gitDir p])
  fixer p = do
    let cmdtxt = [ "export GIT_DIR='"
                 , T.unpack $ gitDir p
                 , "'; "
                 , concat [ "("
                          , "cd $HOME; "
                          , "git init .; "
                          , "git config --local --get-all core.bare true >/dev/null && "
                          , "git config --local --replace-all core.bare false true "
                          , ")"
                          ]
                 ]
    result <- cmd (exe "bash" "-c" $ concat cmdtxt )
    case result of
      Right _ -> putStrLn' "home dir git tracking setup successfully"
      Left err -> putStrLn' $ "Failed setting up home dir git tracking: " <> tshow err

  dependencies _ = return [IsProp HasGitP]

data DotfileConfig = DotfileConfig
  { src :: Text
  , dest :: Maybe Text  -- ^ Optional destination path; if absolute, used directly
  , dot :: Bool
  , sort :: DotfileSort
  , dir :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DotfileSort
  = Symlink
  | Copy
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Describes the difference between desired and current filesystem state for a dotfile
data DotfileDiff
  = DotfileCorrect              -- ^ Already in the desired state, no action needed
  | DotfileMissing              -- ^ Destination doesn't exist, needs to be created
  | DotfileBrokenSymlink        -- ^ Destination is a broken symlink, needs removal and recreation
  | DotfileWrong                -- ^ Destination exists but has wrong content/type, needs backup and recreation
  | DotfileSrcMissing Text      -- ^ Error: source file doesn't exist (carries the missing path)
  deriving (Eq, Show)

-- | Compute the filesystem diff for a single dotfile.
-- This determines what action (if any) is needed to bring the dotfile to the desired state.
computeDotfileDiff :: MonadIO m => DotfileConfig -> Text -> Text -> m DotfileDiff
computeDotfileDiff f src dest = do
  -- Check if source exists
  srcExists <- isRight <$> cmd (exe "bash" "-c" $ "test -e " <> T.unpack src)
  if not srcExists
    then pure $ DotfileSrcMissing src
    else do
      -- Check if dest is a symlink (regardless of whether target exists)
      isLink <- isRight <$> cmd (exe "bash" "-c" $ "test -L " <> T.unpack dest)
      -- Check if dest exists (follows symlinks, so broken symlink = False)
      destExists <- isRight <$> cmd (exe "bash" "-c" $ "test -e " <> T.unpack dest)

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


data DotfilesP = DotfilesP
  { srcDir :: Text
  , destDir :: Maybe Text  -- ^ Destination base directory, defaults to "~/" if Nothing
  , files :: [DotfileConfig]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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
checkSingleDotfile :: MonadIO m => DotfileConfig -> Text -> Text -> m Bool
checkSingleDotfile f src dest = do
  case f.sort of
    Symlink -> do
      -- Check if dest is a symlink pointing to src
      target <- cmd (exe "readlink" (T.unpack dest) |> captureTrim)
      pure $ either (const False) (\t -> TL.toStrict (TL.decodeUtf8 t) == src) target
    Copy -> do
      -- Ensure dest is not a symlink, and contents match
      isSymlink <- isRight <$> cmd (exe "bash" "-c" $ concat ["test -L ", T.unpack dest])
      if isSymlink
        then pure False  -- Wrong type: should be regular file, not symlink
        else do
          diffResult <- cmd (exe "diff" "-rq" (T.unpack src) (T.unpack dest) &> devNull)
          pure $ isRight diffResult

-- | Compute expanded src and dest paths for a dotfile config
computeDotfilePaths :: MonadIO m => DotfilesP -> DotfileConfig -> m (Text, Text)
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
      void $ cmd (exe "rm" (T.unpack dest))
    DotfileWrong -> do
      putStrLn' $ "Backing up existing file: " <> dest
      mvToBackup dest
    _ -> pure ()

  -- Create the dotfile (symlink or copy)
  case f.sort of
    Symlink -> do
      putStrLn' $ "Creating symlink: " <> dest <> " -> " <> src
      void $ cmd (exe "ln" "-s" (T.unpack src) (T.unpack dest))
    Copy -> do
      putStrLn' $ "Copying: " <> src <> " -> " <> dest
      void $ cmd (exe "cp" "-r" (T.unpack src) (T.unpack dest))

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

data BasicSetupP = BasicSetupP
 deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop BasicSetupP where
  desc _ = "basic setup"
  attrs _ = mempty
  checker _ = return True -- dummy prop, wrapper for dependencies
  fixer _ = return () -- all action in dependencies
  dependencies _ = return [ (IsProp HasGitP) ]

data GitMacOSP = GitMacOSP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitMacOSP where
  desc _ = "git (macOS via Homebrew)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd (exe "brew" "install" "git" &> devNull)
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = return [(IsProp HomebrewP)]


data GitDebianP = GitDebianP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop GitDebianP where
  desc _ = "git (Debian via apt)"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  fixer _ = do
    result <- cmd(exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
    case result of
      Right _ -> putStrLn' "Git installed successfully"
      Left err -> putStrLn' $ "Failed to install git: " <> tshow err
  dependencies _ = do
    hasCmd' "git" >>= bool (return [(IsProp AptUpdateP)]) (return [])

data HasGitP = HasGitP
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Prop HasGitP where
  desc _ = "has command `git` installed"
  attrs _ = mempty
  checker _ = hasCmd' "git"
  dependencies _ = do
    os <- detectOS
    case os of
      MacOS -> return [(IsProp HomebrewP)]
      Debian -> return [(IsProp AptUpdateP)]
      Unknown -> error "error: Unknown OS, unable to install git"
  fixer _ = do
    os <- detectOS
    case os of
      Unknown -> error "error: Unknown OS, unable to install git"
      MacOS -> do
        result <- cmd (exe "brew" "install" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err
      Debian -> do
        result <- cmd (exe "sudo" "apt-get" "install" "-y" "git" &> devNull)
        case result of
          Right _ -> putStrLn' "Git installed successfully"
          Left err -> error $ "Failed to install git: " ++ show err

data XCodeCLIToolsP = XCodeCLIToolsP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop XCodeCLIToolsP where
  desc _ = "Xcode CLI Tools"
  attrs _ = mempty
  checker _ = isRight <$> cmd (exe "pkgutil" "--pkg-info=com.apple.pkg.CLTools_Executables" &> devNull)
  fixer _ = do
      putStrLn' "Installing Xcode CLI Tools..."

      -- Create marker file that triggers CLT in softwareupdate list
      void $ cmd (exe "touch" "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

      -- Find and install the CLT package
      let findAndInstall = "softwareupdate -i \"$(softwareupdate -l 2>&1 | grep -o 'Command Line Tools for Xcode-[0-9.]*' | head -1)\""
      result <- cmd (exe "bash" "-c" findAndInstall)

      -- Clean up marker
      -- TODO bracket to clean up
      void $ cmd (exe "rm" "-f" "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

      case result of
        Left err -> error $ "Failed to install Xcode CLI tools: " <> show err
        Right _ -> do
          putStrLn' "Xcode CLI tools installed successfully"

  dependencies _ = return []

data HomebrewP = HomebrewP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop HomebrewP where
  desc _ = "homebrew package manager for macOS"
  attrs _ = mempty
  checker _ = hasCmd' "brew"
  fixer _ = do
    result <- cmd (exe "bash" "-c" "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"  &> devNull)
    case result of
      Right _ -> putStrLn' "Homebrew installed successfully"
      Left err -> putStrLn' $ "Failed to install homebrew: " <> tshow err
  dependencies _ = return [ (IsProp XCodeCLIToolsP) ]

data AptUpdateP = AptUpdateP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop AptUpdateP where
  desc _ = "apt package lists updated"
  attrs _ = mempty
  checker _ = return False  -- Always run update to be safe
  fixer _ = do
   result <- cmd (exe "sudo" "apt-get" "update" &> devNull)
   case result of
     Right _ -> putStrLn' "apt package sets updated successfully"
     Left err -> putStrLn' $ "Failed to update apt: " <> tshow err
  dependencies _ = return []

data WSConfigDirP = WSConfigDirP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Prop WSConfigDirP where
  desc _ = "wshs configuration directory"
  attrs _ = mempty
  checker _ = do
    cfg <- asks configuration
    isRight <$> cmd (exe "bash" "-c" $ concat ["test -d ", T.unpack $ cfg.configDir])
  fixer _ = do
    cfg <- asks configuration
    let cloneCmd =
          intercalate " "
            [ "git clone"
            , "--branch"
            , T.unpack cfg.configRepoBranch
            , T.unpack cfg.configRepoUrl
            , T.unpack cfg.configDir
            ]
    result <- cmd (exe "bash" "-c" $ cloneCmd)
    case result of
      Right _ -> putStrLn' $ "Cloned repository to " <> cfg.configDir
      Left err -> putStrLn' $ "Failed to clone repository: " <> tshow err
  dependencies _ = return [IsProp HasGitP]

-- | Restart the Nix daemon (OS-aware)
restartNixDaemon :: MonadIO m => m ()
restartNixDaemon = do
  os <- detectOS
  case os of
    MacOS -> do
      liftIO $ putStrLn "Restarting Nix daemon (macOS)..."
      void $ cmd (exe "sudo" "launchctl" "unload" "/Library/LaunchDaemons/org.nixos.nix-daemon.plist")
      void $ cmd (exe "sudo" "launchctl" "load" "/Library/LaunchDaemons/org.nixos.nix-daemon.plist")
    Debian -> do
      liftIO $ putStrLn "Restarting Nix daemon (Debian)..."
      void $ cmd (exe "sudo" "systemctl" "restart" "nix-daemon.service")
    Unknown -> error "Cannot restart nix daemon: unknown OS"

data NixDaemonP = NixDaemonP
  { version :: Maybe Text      -- ^ Nix version to install, defaults to "2.24.14"
  , interactive :: Bool        -- ^ If True, allow user to answer installer prompts; if False, pass --yes
  , nixConf :: Maybe Text      -- ^ Desired contents of /etc/nix/nix.conf (optional)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultNixVersion :: Text
defaultNixVersion = "2.24.14"

nixDaemonProfile :: FilePath
nixDaemonProfile = "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"

instance Prop NixDaemonP where
  desc _ = "Nix package manager (daemon mode)"
  attrs p = Map.fromList
    [ ("version", fromMaybe defaultNixVersion p.version)
    , ("interactive", tshow p.interactive)
    ]
  checker p = do
    nixInstalled <- hasCmd' "nix"
    if not nixInstalled
      then return False
      else case p.nixConf of
        Nothing -> return True  -- No config specified, nix installed = good
        Just desiredConf -> do
          let nixConfPath = "/etc/nix/nix.conf"
          fileContentsCheck nixConfPath desiredConf
  fixer p = do
    -- Check if Nix is already installed
    nixInstalled <- hasCmd' "nix"

    if nixInstalled
      then putStrLn' "Nix already installed, checking configuration..."
      else installNix

    -- Manage nix.conf if specified (runs whether or not we just installed)
    updateNixConf

    putStrLn' "Nix daemon setup complete."

    where
      installNix :: WS ()
      installNix = do
        let ver = fromMaybe defaultNixVersion p.version
        let installerUrl = "https://releases.nixos.org/nix/nix-" <> ver <> "/install"
        let yesFlag = if p.interactive then "" else " --yes"

        putStrLn' $ "Installing Nix " <> ver <> "..."

        let installCmd = "curl -L " <> T.unpack installerUrl <> " | sh -s -- --daemon" <> T.unpack yesFlag
        result <- cmd (exe "bash" "-c" installCmd)
        case result of
          Left err -> error $ "Nix installation failed: " <> show err
          Right _ -> pure ()

        -- Verify profile exists
        profileExists <- isRight <$> cmd (exe "test" "-e" nixDaemonProfile)
        unless profileExists $
          error $ "Nix installed, but cannot find profile file: " <> nixDaemonProfile

        putStrLn' ""
        putStrLn' "Nix installed. Add the following to your shell profile:"
        putStrLn' $ "  . " <> T.pack nixDaemonProfile
        putStrLn' ""

        restartNixDaemon

      updateNixConf :: WS ()
      updateNixConf = case p.nixConf of
        Nothing -> pure ()
        Just desiredConf -> do
          let nixConfPath = "/etc/nix/nix.conf"
          result <- fileContentsFix nixConfPath desiredConf
          case result of
            Nothing -> putStrLn' "  nix.conf already has correct contents"
            Just backupPath -> do
              putStrLn' $ "  Updated " <> nixConfPath
              when (backupPath /= "") $
                putStrLn' $ "  (backed up original to " <> backupPath <> ")"
              -- Restart daemon since config changed
              restartNixDaemon

  dependencies _ = return []

getProp :: Property -> IsProp
getProp (GitHomeDir p) = IsProp p
getProp (Dotfiles p) = IsProp p
getProp (NixDaemon p) = IsProp p

bootstrapParser :: Parser Command
bootstrapParser = Bootstrap
  <$> strArgument
      ( metavar "CONFIG"
     <> help "Path to the configuration YAML file" )
  <*> strArgument
      ( metavar "WORKSTATION"
     <> help "Name of the current workstation" )

nixSubcommandParser :: Parser NixSubcommand
nixSubcommandParser = subparser
  ( App.command "restart"
    ( info (pure NixRestart <**> helper)
      ( progDesc "Restart the Nix daemon" )
    )
  )

commandParser :: Parser Command
commandParser = subparser
  ( App.command "bootstrap"
    ( info (bootstrapParser <**> helper)
      ( progDesc "Bootstrap a new workstation" )
    )
 <> App.command "nix"
    ( info (Nix <$> nixSubcommandParser <**> helper)
      ( progDesc "Nix package manager utilities" )
    )
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> switch
      ( long "sudo-cache"
     <> help "Cache sudo credentials and refresh in background (prompts once at start)"
      )
  <*> optional (T.pack <$> strOption
      ( long "sudo-pass-file"
     <> metavar "FILE"
     <> help "File containing sudo password (first line only, implies --sudo-cache)"
      ))


parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "WSHS - Workstation Setup Helper System"
      <> header "wshs - manage workstation configurations"
    )

ensureProperty :: Prop p => p -> WS ()
ensureProperty prop = do
  wsstate <- get
  let
    seen :: Set IsProp
    seen = wsstate.props
  if Set.member (IsProp prop) seen
    then return ()
    else do
      -- First, ensure all dependencies
      -- here should print the reason a prop is beign executed
      --  ("A depends on B, checking A")
      deps <- dependencies prop
      -- TODO handle circular dependencies possibility
      forM_ deps $ ensureProperty

      -- Now check and fix this property
      putStrLn' $ "Checking property: " <> desc prop
      isValid <- checker prop
      if isValid
        then putStrLn' "  ✓ Already valid"
        else do
          putStrLn' "  ✗ Invalid, applying fix..."
          fixer prop

      put $ wsstate { props = Set.insert (IsProp prop) seen}

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
  -- Use <<< to pipe password to sudo -S (reads password from stdin)
  isRight <$> tryFailure (exe "sudo" "-S" "-p" "" "-v" <<< LBS.pack (T.unpack pass <> "\n"))

-- | Start a background thread that refreshes sudo credentials every 60 seconds.
-- Returns the ThreadId so it can be killed when done.
-- Uses Nothing for password since credentials are already cached.
startSudoRefreshLoop :: IO ThreadId
startSudoRefreshLoop = forkIO $ forever $ do
  threadDelay (60 * 1000000)  -- 60 seconds in microseconds
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

main :: IO ()
main = do
  opts <- parseOptions

  -- Initialize sudo caching if requested
  sudoThread <- if opts.sudoCache
    then initSudoCache opts.sudoPassFile
    else pure Nothing

  case opts.command of
    Bootstrap cfgPath ws -> do
      cfg <- decodeFileThrow cfgPath :: IO Configuration
      void $ flip runStateT (WSState { props = mempty }) $ flip runReaderT (Settings opts cfg) $ unWS $ do
        liftIO $ print cfg
        putStrLn' $ "Workstation: " <> ws
        putStrLn' "\nEnsuring properties..."
        ensureProperty (IsProp BasicSetupP)
        ensureProperty (IsProp WSConfigDirP)
        forM_ (getProp <$> cfg.properties) ensureProperty
    Nix NixRestart ->
      restartNixDaemon

  -- Clean up sudo refresh thread
  case sudoThread of
    Just tid -> killThread tid
    Nothing -> pure ()
