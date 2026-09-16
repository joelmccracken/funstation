{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Properties.BitwardenSecretsSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesPathExist, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (bracket, bracket_, try, throwIO, IOException)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import Funstation.Types (OS(..), WSError(..), checker)
import Funstation.Commands (hasCmd', readFileIfExists)
import Funstation.Properties.BitwardenSecrets
import TestHelpers

-- | Run an action with @$HOME@ pointed at a fresh temp dir holding a
-- @secrets/@ directory, restoring the real value afterwards.
--
-- Mutates global state, so tests must be run sequentially.
withTempHome :: (FilePath -> IO a) -> IO a
withTempHome fn =
  -- would consider using MonadReader to store/retrieve home
  -- but that seems like any invoked shell commands might cause bizarre issues
  withTempDir "funstation-bw-test" $ \tmpDir ->
    bracket
      (lookupEnv "HOME" <* setEnv "HOME" tmpDir)
      (maybe (unsetEnv "HOME") (setEnv "HOME"))
      (\_ -> do
          createDirectoryIfMissing True (tmpDir </> "secrets")
          fn tmpDir)

-- | Populate credential files for test
writeAllCreds :: FilePath -> IO ()
writeAllCreds home = do
  let creds = bwCredPaths home
  writeFile creds.clientIdFile "user.deadbeef"
  writeFile creds.clientSecretFile "s3cret"
  writeFile creds.masterPassFile "hunter2"

testProp :: BitwardenSecretsP
testProp = BitwardenSecretsP { syncIntervalDays = 7 }

-- | Run 'withEnvVar' over an action that throws, for asserting on cleanup and
-- on the propagated error. The result type is fixed here so call sites do not
-- each need an annotation.
throwingWithEnvVar :: IO (Either WSError ())
throwingWithEnvVar =
  runWSEither MacOS $
    withEnvVar "FUNSTATION_TEST_VAR" "value" $
      throwError (WSFailure "boom")

spec :: Spec
spec = do
  describe "bwCredPaths" $ do
    it "derives all three credential paths from a home directory" $ do
      let creds = bwCredPaths "/home/user"
      creds.clientIdFile `shouldBe` "/home/user/secrets/bw_client_id"
      creds.clientSecretFile `shouldBe` "/home/user/secrets/bw_client_secret"
      creds.masterPassFile `shouldBe` "/home/user/secrets/bw_master_pass"

  describe "readCredFile" $ do
    it "reads and strips a credential" $ withTempHome $ \home -> do
      let path = home </> "secrets" </> "bw_client_id"
      writeFile path "user.deadbeef\n"
      shouldBeM "user.deadbeef" $ runWS $ readCredFile path

    it "fails, naming the file, when it is missing" $ withTempHome $ \home -> do
      let path = home </> "secrets" </> "bw_client_id"
      result <- runWSEither MacOS $ readCredFile path
      case result of
        Left (WSFailure msg) -> msg `shouldSatisfy` T.isInfixOf "bw_client_id"
        other -> expectationFailure $ "expected a WSFailure, got: " <> show other

    it "fails when the file is empty" $ withTempHome $ \home -> do
      let path = home </> "secrets" </> "bw_client_secret"
      writeFile path "\n"
      result <- runWSEither MacOS $ readCredFile path
      result `shouldSatisfy` isLeft

  describe "readFileIfExists" $ do
    it "returns stripped contents when the file exists" $ withTempHome $ \home -> do
      let path = home </> "afile"
      writeFile path "  contents \n"
      shouldBeM (Just "contents") $ runWS $ readFileIfExists path

    it "returns Nothing when the file is absent" $ withTempHome $ \home ->
      shouldBeM Nothing $ runWS $ readFileIfExists (home </> "nope")

  describe "getLastSyncTs" $ do
    let writeTs home s = do
          let dir = home </> ".local/state/funstation/bitwarden-secrets"
          createDirectoryIfMissing True dir
          writeFile (dir </> "last-sync-ts") s

    it "reads a stored timestamp" $ withTempHome $ \home -> do
      writeTs home "1700000000\n"
      shouldBeM 1700000000 $ getLastSyncTs home

    it "returns 0 when no sync has been recorded" $ withTempHome $ \home ->
      shouldBeM 0 $ getLastSyncTs home

    it "returns 0 for a corrupt state file rather than crashing" $ withTempHome $ \home -> do
      writeTs home "not a number\n"
      shouldBeM 0 $ getLastSyncTs home

    it "returns 0 for an empty state file" $ withTempHome $ \home -> do
      writeTs home ""
      shouldBeM 0 $ getLastSyncTs home

  describe "isSyncFresh" $ do
    let day = 60 * 60 * 24
    it "is fresh immediately after a sync" $
      isSyncFresh 7 1000 1000 `shouldBe` True

    it "is fresh just inside the interval" $
      isSyncFresh 7 1000 (1000 + 7 * day - 1) `shouldBe` True

    it "is fresh exactly at the interval boundary" $
      isSyncFresh 7 1000 (1000 + 7 * day) `shouldBe` True

    it "is stale one second past the interval" $
      isSyncFresh 7 1000 (1000 + 7 * day + 1) `shouldBe` False

    it "is stale when no sync has ever happened" $
      isSyncFresh 7 0 (100 * day) `shouldBe` False

    it "respects a shorter interval" $
      isSyncFresh 1 1000 (1000 + 2 * day) `shouldBe` False

  describe "fileItemTargets" $ do
    let item n c = BwItem { id = "id", name = n, notes = c }

    it "strips the file: prefix and keeps contents" $
      fileItemTargets [item "file:~/.ssh/config" (Just "contents")]
        `shouldBe` [("~/.ssh/config", Just "contents")]

    it "ignores items without the file: prefix" $
      fileItemTargets [item "some login" (Just "x"), item "file:~/a" (Just "y")]
        `shouldBe` [("~/a", Just "y")]

    it "keeps items with no contents, so they can be reported as skipped" $
      fileItemTargets [item "file:~/a" Nothing] `shouldBe` [("~/a", Nothing)]

    it "returns nothing for an empty vault" $
      fileItemTargets [] `shouldBe` []

    it "does not treat a mid-name file: as a prefix" $
      fileItemTargets [item "my file:thing" (Just "x")] `shouldBe` []

  describe "JSON decoding" $ do
    it "parses a folder listing" $
      parseFolders "[{\"id\":\"f1\",\"name\":\"bww_files\"}]"
        `shouldBe` Right [BwFolder { id = "f1", name = "bww_files" }]

    it "parses a single folder creation response" $
      parseFolder "{\"id\":\"f2\",\"name\":\"bww_files\"}"
        `shouldBe` Right (BwFolder { id = "f2", name = "bww_files" })

    it "parses items, including a null notes field" $
      parseItems "[{\"id\":\"i1\",\"name\":\"file:~/a\",\"notes\":null}]"
        `shouldBe` Right [BwItem { id = "i1", name = "file:~/a", notes = Nothing }]

    it "reports malformed folder JSON as an error" $
      parseFolders "not json" `shouldSatisfy` isLeft

    it "reports malformed item JSON as an error" $
      parseItems "{\"id\":\"i1\"}" `shouldSatisfy` isLeft

  describe "writeFileItems" $ do
    it "writes contents to the target path" $ withTempHome $ \home -> do
      let dest = home </> "written-secret"
      runWS $ writeFileItems [(T.pack dest, Just "secret contents")]
      shouldBeM "secret contents" $ readFile dest

    it "creates a missing parent directory" $ withTempHome $ \home -> do
      let dest = home </> ".ssh" </> "config"
      runWS $ writeFileItems [(T.pack dest, Just "contents")]
      shouldBeM "contents" $ readFile dest

    it "skips items with no contents" $ withTempHome $ \home -> do
      let dest = home </> "not-written"
      runWS $ writeFileItems [(T.pack dest, Nothing)]
      shouldBeM False $ doesPathExist dest

    it "writes several items at once" $ withTempHome $ \home -> do
      let destA = home </> "a"
          destB = home </> "b"
      runWS $ writeFileItems [(T.pack destA, Just "aaa"), (T.pack destB, Just "bbb")]
      shouldBeM "aaa" $ readFile destA
      shouldBeM "bbb" $ readFile destB

  describe "withEnvVar" $ do
    it "sets the variable for the duration of the action" $
      shouldBeM (Just "value") $ runWS $
        withEnvVar "FUNSTATION_TEST_VAR" "value" $
          liftIO $ lookupEnv "FUNSTATION_TEST_VAR"

    it "unsets the variable on the success path" $ do
      runWS $ withEnvVar "FUNSTATION_TEST_VAR" "value" $ return ()
      shouldBeM Nothing $ lookupEnv "FUNSTATION_TEST_VAR"

    it "restores a prior value rather than unsetting it" $
      bracket_ (setEnv "FUNSTATION_TEST_VAR" "original")
               (unsetEnv "FUNSTATION_TEST_VAR") $ do
        runWS $ withEnvVar "FUNSTATION_TEST_VAR" "temporary" $ return ()
        shouldBeM (Just "original") $ lookupEnv "FUNSTATION_TEST_VAR"

    it "restores a prior value even when the action throws" $
      bracket_ (setEnv "FUNSTATION_TEST_VAR" "original")
               (unsetEnv "FUNSTATION_TEST_VAR") $ do
        _ <- throwingWithEnvVar
        shouldBeM (Just "original") $ lookupEnv "FUNSTATION_TEST_VAR"

    it "restores the prior value when an async exception interrupts the action" $
      bracket_ (setEnv "FUNSTATION_TEST_VAR" "original")
               (unsetEnv "FUNSTATION_TEST_VAR") $ do
        -- bracket, unlike the catchError it replaced, also unwinds here.
        result <- try $ runWS $ withEnvVar "FUNSTATION_TEST_VAR" "temporary" $
          liftIO $ throwIO (userError "boom")
        case result of
          Left e  -> show (e :: IOException) `shouldContain` "boom"
          Right _ -> expectationFailure "expected the exception to propagate"
        shouldBeM (Just "original") $ lookupEnv "FUNSTATION_TEST_VAR"

    it "unsets the variable when the action throws" $ do
      result <- throwingWithEnvVar
      result `shouldSatisfy` isLeft
      shouldBeM Nothing $ lookupEnv "FUNSTATION_TEST_VAR"

    it "propagates the original error" $ do
      result <- throwingWithEnvVar
      result `shouldBe` Left (WSFailure "boom")
      

  describe "withApiKeyEnv" $ do
    it "exposes the API key via the environment" $ withTempHome $ \home -> do
      writeAllCreds home
      shouldBeM (Just "user.deadbeef", Just "s3cret") $ runWS $
        withApiKeyEnv (bwCredPaths home) $ liftIO $
          (,) <$> lookupEnv "BW_CLIENTID" <*> lookupEnv "BW_CLIENTSECRET"

    it "clears both variables afterwards" $ withTempHome $ \home -> do
      writeAllCreds home
      runWS $ withApiKeyEnv (bwCredPaths home) $ return ()
      shouldBeM Nothing $ lookupEnv "BW_CLIENTID"
      shouldBeM Nothing $ lookupEnv "BW_CLIENTSECRET"

    it "fails when the client id is missing" $ withTempHome $ \home -> do
      writeAllCreds home
      removeFile (bwCredPaths home).clientIdFile
      result <- runWSEither MacOS $ withApiKeyEnv (bwCredPaths home) $ return ()
      result `shouldSatisfy` isLeft

  -- The credential precondition is tested directly rather than only through
  -- 'checker', because 'checker' short-circuits on @hasCmd' "bw"@ and would
  -- return False for the wrong reason on a machine without bw installed.
  describe "credsPresent" $ do
    it "is True when all three files exist" $ withTempHome $ \home -> do
      writeAllCreds home
      shouldBeM True $ credsPresent (bwCredPaths home)

    it "is False when no credential files exist" $ withTempHome $ \home ->
      shouldBeM False $ credsPresent (bwCredPaths home)

    it "is False when the client id is absent" $ withTempHome $ \home -> do
      writeAllCreds home
      removeFile (bwCredPaths home).clientIdFile
      shouldBeM False $ credsPresent (bwCredPaths home)

    it "is False when the client secret is absent" $ withTempHome $ \home -> do
      writeAllCreds home
      removeFile (bwCredPaths home).clientSecretFile
      shouldBeM False $ credsPresent (bwCredPaths home)

    it "is False when the master password is absent" $ withTempHome $ \home -> do
      writeAllCreds home
      removeFile (bwCredPaths home).masterPassFile
      shouldBeM False $ credsPresent (bwCredPaths home)

  describe "checker" $ do
    -- checker's first act is `hasCmd' "bw"`, so without bw on PATH every
    -- assertion below would pass trivially. Skip rather than pretend.
    bwAvailable <- runIO $ runWS $ hasCmd' "bw"
    let requiresBw act = if bwAvailable then act else pendingWith "bw not installed"

    it "returns False when no credential files exist" $ withTempHome $ \_ ->
      requiresBw $ shouldBeM False $ runWS $ checker testProp

    it "returns False when only some credential files exist" $ withTempHome $ \home -> do
      writeAllCreds home
      removeFile (bwCredPaths home).clientSecretFile
      requiresBw $ shouldBeM False $ runWS $ checker testProp

    it "returns False when creds exist but the last sync is stale" $ withTempHome $ \home -> do
      writeAllCreds home
      let stateDir = home </> ".local/state/funstation/bitwarden-secrets"
      createDirectoryIfMissing True stateDir
      writeFile (stateDir </> "last-sync-ts") "1\n"
      requiresBw $ shouldBeM False $ runWS $ checker testProp
