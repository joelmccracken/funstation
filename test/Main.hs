{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main, main2) where

import Test.Hspec
import Text.RawString.QQ
import Data.Text (Text)
import Control.Monad.IO.Class
import Shh.Internal (exe, captureTrim, (|>), tryFailure, toArgs, asArg, displayCommand)
import Data.ByteString.Lazy hiding (writeFile, readFile, length)

import qualified SudoSpec
import qualified WorkstationSpec
import qualified UtilsSpec
import qualified Properties.GitHomeDirSpec
import qualified Properties.GitCloneSpec
import qualified Properties.DotfilesSpec

configText :: Text
configText = [r|
properties:
- type: GitHomeDir
  params:
    gitDir: ".git-homedir"
|]



main2 :: IO ()
main2 = hspec $ do
  it "experimentation space" $ do
    liftIO $ tryFailure $ toArgs (asArg "env") "touch" "foobar"
    liftIO $ tryFailure $ exe ["env", "bash", "-c", "echo something great > foobar2"]
    capt <- liftIO $ ( exe ["env", "cat", "foobar2"] |> captureTrim)

    capt `shouldBe` "something great"

    let pth = "foobar2"
    foo <- liftIO $ do
      let x = ["echo", "the file is '", capt, "' at filen named ", pth ]
      useSudo <- pure False
      putStrLn "before"
      if useSudo
        then pure ((exe $ "sudo" : x) :: [ByteString])
        else pure (exe $ "env" : x)
    putStrLn $ show $ displayCommand $ exe foo
    putStrLn "before1"
    _ <- exe foo
    putStrLn "after1"

    -- userCmd <- mkPrivCmd "sudo" WriteAccess "root_only" ["bash", "-c", "echo $USER"]
    -- user <- liftIO $ userCmd |> captureTrim
    -- -- TL.putStrLn $ TL.decodeLatin1 res
    -- user `shouldBe` "root"

main :: IO ()
main = hspec $ do
  UtilsSpec.spec
  SudoSpec.spec
  WorkstationSpec.spec
  Properties.GitHomeDirSpec.spec
  Properties.GitCloneSpec.spec
  Properties.DotfilesSpec.spec
