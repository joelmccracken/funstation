{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main, main2) where

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
import Funstation hiding (main, failLeft)
import Control.Monad (forM_)
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Set qualified as Set
import Shh.Internal (exe, devNull, (&>), Proc, Failure, captureTrim, (|>), tryFailure, (<<<), toArgs, asArg, displayCommand, Cmd)
import Data.ByteString.Lazy hiding (writeFile, readFile, length)
import Data.Either (isRight)
import qualified SudoSpec
import qualified GitHomeDirSpec
import qualified GitCloneSpec
import qualified WorkstationSpec
import qualified DotfilesSpec
import Util
import Funstation.Proc

-- | Run a WS action with a minimal configuration
-- TODO this really should take the cfg opts settings and initial state
-- somehow, or have some other way to have it parameterized. The fact that whatever
-- WS prop that is using this function doesn't need to have those things set is not
-- a stable property. Figure that out at some point.
runWS :: WS a -> IO a
runWS action = do
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = "", workstation = Nothing }
  let settings = Settings { opts = opts, sudoCmd = "sudo", workstation = "workstation" }
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) settings)) initialState

-- | Run a WS action with a specific sudoCmd in Settings
runWSWith :: String -> WS a -> IO a
runWSWith sc action = do
  let opts = Options { command = Bootstrap, sudoCache = False, sudoPassFile = Nothing, verbose = False, interactive = False, configPath = "", workstation = Nothing }
  let settings = Settings { opts = opts, sudoCmd = sc, workstation = "workstation" }
  let initialState = WSState { props = Set.empty }
  failLeft . fst =<< runStateT (runExceptT (runReaderT (unWS action) settings)) initialState

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
  DotfilesSpec.spec

  describe "fileContentsCheck" $ do
    let withTempDir fn =
          withSystemTempDirectory "funstation-test" $ \tmpDir -> fn tmpDir

    it "returns True when file exists with matching contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "test content\nline 2\n"
      writeFile testFile content
      result <- runWS $ fileContentsCheck (T.pack testFile) (T.pack content)
      result `shouldBe` True

    it "returns False when file exists with different contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      result <- runWS $ fileContentsCheck (T.pack testFile) "different content"
      result `shouldBe` False

    it "returns False when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "nonexistent"
      result <- runWS $ fileContentsCheck (T.pack testFile) "some content"
      result `shouldBe` False

    it "returns True for empty file with empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      result <- runWS $ fileContentsCheck (T.pack testFile) ""
      result `shouldBe` True

    it "returns False for empty file with non-empty desired content" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "emptyfile"
      writeFile testFile ""
      result <- runWS $ fileContentsCheck (T.pack testFile) "some content"
      result `shouldBe` False

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let content = "line 1\nline 2\nline 3\n"
      writeFile testFile content
      result <- runWS $ fileContentsCheck (T.pack testFile) (T.pack content)
      result `shouldBe` True

  describe "fileContentsFix" $ do
    let withTempDir fn =
          withSystemTempDirectory "funstation-test" $ \tmpDir -> fn tmpDir

    it "returns Nothing when file already has correct contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let content = "correct content"
      writeFile testFile content
      result <- runWS $ fileContentsFix (T.pack testFile) (T.pack content)
      result `shouldBe` Nothing
      -- Verify no backup was created
      files <- listDirectory tmpDir
      length files `shouldBe` 1

    it "returns Just backupPath when file exists with wrong contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      writeFile testFile "original content"
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      result `shouldSatisfy` \case
        Just path -> not (T.null path)
        Nothing -> False
      -- Verify file was updated
      newContent <- readFile testFile
      newContent `shouldBe` "new content"
      -- Verify backup exists
      files <- listDirectory tmpDir
      length files `shouldBe` 2  -- original + backup

    it "returns Just empty string when file does not exist" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "newfile"
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      result `shouldBe` Just ""
      -- Verify file was created
      exists <- doesPathExist testFile
      exists `shouldBe` True
      content <- readFile testFile
      content `shouldBe` "new content"

    it "backup file contains original contents" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "testfile"
      let originalContent = "original content"
      writeFile testFile originalContent
      result <- runWS $ fileContentsFix (T.pack testFile) "new content"
      case result of
        Just backupPath -> do
          backupContent <- readFile (T.unpack backupPath)
          backupContent `shouldBe` originalContent
        Nothing -> expectationFailure "Expected Just backupPath"

    it "handles multiline content correctly" $ withTempDir $ \tmpDir -> do
      let testFile = tmpDir </> "multiline"
      let originalContent = "line 1\nline 2\n"
      let newContent = "new line 1\nnew line 2\nnew line 3\n"
      writeFile testFile originalContent
      result <- runWS $ fileContentsFix (T.pack testFile) (T.pack newContent)
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False
      -- Verify new content
      updatedContent <- readFile testFile
      updatedContent `shouldBe` newContent

  describe "mkPrivCmd" $ do
    let withTempDir fn = withSystemTempDirectory "funstation-test" $ \tmpDir -> fn tmpDir

    it "uses env prefix when path is user-owned (no sudo needed)" $ withTempDir $ \tmpDir -> do
      -- Path is user-owned → needsSudo returns False → exe ("env" : args)
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "sudo" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo hello > " <> outFile]
      _ <- exe (T.unpack <$> args)
      content <- readFile outFile
      content `shouldBe` "hello\n"

    it "uses injected sudo command when needs check returns True (injected as env)" $ withTempDir $ \tmpDir -> do
      -- inject "env" as the sudo command so the command succeeds even if sudo branch is taken
      let outFile = tmpDir </> "out.txt"
      args <- mkPrivCmd "env" WriteAccess (T.pack tmpDir) ["bash", "-c", T.pack $ "echo injected > " <> outFile]
      _ <- exe (T.unpack <$> args)
      content <- readFile outFile
      content `shouldBe` "injected\n"

    it "returned Cmd can be chained with |> to capture output" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "captured content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result <- exe (T.unpack <$> args) |> captureTrim
      result `shouldBe` "captured content"

    it "returned Cmd can be chained with &> devNull" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "some content"
      args <- mkPrivCmd "sudo" ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result <- tryFailure $ exe (T.unpack <$> args) &> devNull
      result `shouldSatisfy` isRight

  describe "privCmd" $ do
    let withTempDir fn = withSystemTempDirectory "funstation-test" $ \tmpDir -> fn tmpDir

    it "runs WriteAccess command on user-owned path without sudo" $ withTempDir $ \tmpDir -> do
      let outFile = tmpDir </> "out.txt"
      result <- runWS $ privCmd WriteAccess (T.pack tmpDir)
                          ["bash", "-c", T.pack $ "echo write-ok > " <> outFile]
      result `shouldSatisfy` isRight
      content <- readFile outFile
      content `shouldBe` "write-ok\n"

    it "reads sudoCmd from Settings (injected as env) for write path" $ withTempDir $ \tmpDir -> do
      -- sudoCmd = "env" means even if sudo were needed, env is used — command succeeds
      let outFile = tmpDir </> "out.txt"
      result <- runWSWith "env" $ privCmd WriteAccess (T.pack tmpDir)
                                    ["bash", "-c", T.pack $ "echo env-sudo > " <> outFile]
      result `shouldSatisfy` isRight
      content <- readFile outFile
      content `shouldBe` "env-sudo\n"

    it "runs ReadAccess command on user-owned file" $ withTempDir $ \tmpDir -> do
      let srcFile = tmpDir </> "src.txt"
      writeFile srcFile "read-ok"
      result <- runWS $ privCmd ReadAccess (T.pack srcFile) ["cat", T.pack srcFile]
      result `shouldSatisfy` isRight

    it "fails when the command itself fails" $ withTempDir $ \tmpDir -> do
      result <- runWS $ privCmd WriteAccess (T.pack tmpDir) ["false"]
      result `shouldSatisfy` \case
        Left _  -> True
        Right _ -> False

  describe "sudo functionality" $  do
    SudoSpec.spec

  describe "GitHomeDirP" $
    GitHomeDirSpec.spec

  describe "GitCloneP" $
    GitCloneSpec.spec

  describe "workstation names" $
    WorkstationSpec.spec
