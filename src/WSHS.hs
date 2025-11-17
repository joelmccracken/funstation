{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module WSHS (main) where

import qualified Dhall as D
import Options.Applicative
import Options.Applicative qualified as App
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  }
  deriving (D.Generic, Show)


instance D.FromDhall Configuration


data Property = Property
  { name :: Text
  , checker :: IO Bool
  , fixer :: IO ()
  }


data Command
  = Bootstrap
    { configPath :: FilePath
    , workstation :: String
    }
  deriving (Show)


data Options = Options
  { command :: Command
  }
  deriving (Show)


-- have a config.dhall in a known location
-- have a current.dhall at a known location for the "right now" settings
-- current workstation name, etc



bootstrapParser :: Parser Command
bootstrapParser = Bootstrap
  <$> strArgument
      ( metavar "CONFIG"
     <> help "Path to the configuration dhall file" )
  <*> strArgument
      ( metavar "WORKSTATION"
     <> help "Name of the current workstation" )


commandParser :: Parser Command
commandParser = subparser
  ( App.command "bootstrap"
    ( info (bootstrapParser <**> helper)
      ( progDesc "Bootstrap a new workstation" )
    )
  )


optionsParser :: Parser Options
optionsParser = Options <$> commandParser


parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    (  fullDesc
      <> progDesc "WSHS - Workstation Setup Helper System"
      <> header "wshs - manage workstation configurations"
    )


xcodeCliTools :: Property
xcodeCliTools = Property
  { name = "Xcode CLI Tools"
  , checker = do
      (exitCode, _, _) <- readProcessWithExitCode "pkgutil" ["--pkg-info=com.apple.pkg.CLTools_Executables"] ""
      return $ exitCode == ExitSuccess
  , fixer = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "sudo" ["bash", "-c", "(xcodebuild -license accept; xcode-select --install) || exit 0"] ""
      case exitCode of
        ExitSuccess -> putStrLn "Xcode CLI tools installed successfully"
        ExitFailure _ -> do
          putStrLn "Failed to install Xcode CLI tools"
          putStrLn $ "stdout: " ++ stdout
          putStrLn $ "stderr: " ++ stderr
  }


homebrew :: Property
homebrew = Property
  { name = "homebrew"
  , checker = do
      (exitCode, _, _) <- readProcessWithExitCode "which" ["brew"] ""
      return $ exitCode == ExitSuccess
  , fixer = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "sudo" ["bash", "-c", "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"] ""

      case exitCode of
        ExitSuccess -> putStrLn "Homebrew installed successfully"
        ExitFailure _ -> do
          putStrLn "Failed to install homebrew"
          putStrLn $ "stdout: " ++ stdout
          putStrLn $ "stderr: " ++ stderr
  }


git :: Property
git = Property
  { name = "git"
  , checker = do
      (exitCode, _, _) <- readProcessWithExitCode "which" ["brew"] ""
      return $ exitCode == ExitSuccess
  , fixer = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "brew" ["install", "git"] ""

      case exitCode of
        ExitSuccess -> putStrLn "Git installed successfully"
        ExitFailure _ -> do
          putStrLn "Failed to install git"
          putStrLn $ "stdout: " ++ stdout
          putStrLn $ "stderr: " ++ stderr
  }


ensureProperty :: Property -> IO ()
ensureProperty prop = do
  putStrLn $ "Checking property: " ++ T.unpack prop.name
  isValid <- prop.checker
  if isValid
    then putStrLn "  ✓ Already valid"
    else do
      putStrLn "  ✗ Invalid, applying fix..."
      prop.fixer


main :: IO ()
main = do
  opts <- parseOptions
  case opts.command of
    Bootstrap cfgPath ws -> do
      cfg <- D.input D.auto (T.pack cfgPath)
      print (cfg :: Configuration)
      putStrLn $ "Workstation: " ++ ws
      putStrLn "\nEnsuring properties..."
      ensureProperty xcodeCliTools
      ensureProperty homebrew
      ensureProperty git
