{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WSHS (main) where

import qualified Dhall as D
import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T


data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  }
  deriving (D.Generic, Show)


instance D.FromDhall Configuration


data Options = Options
  { configPath :: FilePath
  , workstation :: String
  }
  deriving (Show)


optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      ( metavar "CONFIG"
     <> help "Path to the configuration dhall file" )
  <*> strArgument
      ( metavar "WORKSTATION"
     <> help "Name of the current workstation" )


parseOptions :: IO Options
parseOptions =
  execParser $ info (optionsParser <**> helper)
    (  fullDesc
      <> progDesc "WSHS - Workstation Setup Helper System"
      <> header "wshs - manage workstation configurations"
    )



main :: IO ()
main = do
  opts <- parseOptions
  cfg <- D.input D.auto (T.pack $ configPath opts)
  print (cfg :: Configuration)
  putStrLn $ "Workstation: " ++ workstation opts
