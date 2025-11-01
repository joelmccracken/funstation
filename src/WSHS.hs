{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WSHS (run) where

import Dhall


data Configuration = Configuration
  { dotfilesRepoUrl :: Text
  , dotfilesRepoOrigin :: Text
  , workstationName :: Text
  }
  deriving (Generic, Show)


instance FromDhall Configuration


run :: IO ()
run = putStrLn "someFunc"
