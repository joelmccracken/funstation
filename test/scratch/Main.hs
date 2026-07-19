{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Test.Hspec
import Text.RawString.QQ
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Text.Lazy.Encoding qualified as TL


-- import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)
import Funstation hiding (main)
import Control.Monad (forM_)
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import System.IO.Temp (withSystemTempDirectory)
import Data.Set qualified as Set
import Shh.Internal (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure, (<<<), toArgs, asArg, displayCommand, Cmd)
import Data.ByteString.Lazy hiding (writeFile, readFile, length)

-- | Run a WS action with a minimal configuration
-- TODO this really should take the cfg opts settings and initial state
-- somehow, or have some other way to have it parameterized. The fact that whatever
-- WS prop that is using this function doesn't need to have those things set is not
-- a stable property. Figure that out at some point.
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap "" "", sudoCache = False, sudoPassFile = Nothing, verbose = False }
  let settings = Settings { opts = opts, sudoCmd = "sudo" }
  let initialState = WSState { props = Set.empty }
  (result, _) <- runStateT (runExceptT (runReaderT (unWS action) settings)) initialState
  case result of
    Left (WSFailure msg) -> fail $ "WS action failed: " <> T.unpack msg
    Right a -> pure a

-- | Create a test file with content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = writeFile path content

-- | Create a test directory with a file inside
createTestDir :: FilePath -> IO ()
createTestDir path = do
  createDirectoryIfMissing True path
  writeFile (path </> "testfile.txt") "test content"

configText :: Text
configText = [r|
properties:
- type: GitHomeDir
  params:
    gitDir: ".git-homedir"
|]



main :: IO ()
main = hspec $ do

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

    -- TODO restore this maybe but file should not go in repo dir
    -- maybe do it again after extracting the tmp dir helper

    -- userCmd <- mkPrivCmd "sudo" WriteAccess "root_only" ["bash", "-c", "echo $USER"]
    -- user <- liftIO $ userCmd |> captureTrim
    -- -- TL.putStrLn $ TL.decodeLatin1 res
    -- user `shouldBe` "root"
