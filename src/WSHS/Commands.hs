{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE DeriveAnyClass    #-}
-- {-# LANGUAGE DerivingStrategies    #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RankNTypes #-}

module WSHS.Commands where

-- import Options.Applicative
-- import Options.Applicative qualified as App
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Text qualified as T
-- import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
-- import Data.Either (isRight)
import Shh (exe, tryFailure, devNull, (&>), captureTrim, (|>), Proc, Failure)
import GHC.Stack
-- import Data.Yaml (decodeFileThrow)
-- import qualified Data.Set as Set
-- import Control.Monad (void, forM_)
import Control.Monad.IO.Class
import WSHS.Types
-- import WSHS.Types.Configuration
-- import Control.Monad.State
-- import Control.Monad.Reader


cmd :: Proc a -> WS (Either Failure a)
cmd c = liftIO $ tryFailure $ withFrozenCallStack c

detectOS :: WS OS
detectOS = do
  osCheck  <- cmd (exe "uname" "-s" |> captureTrim)
  case osCheck of
    Left e -> error $ "error detecting OS: " <> show e
    Right "Darwin" -> return MacOS
    Right "Linux" -> detectLinuxOS
    Right _ -> return Unknown

detectLinuxOS :: WS  OS
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

hasCmd :: Text -> WS Bool
hasCmd cmdName = isJust <$> which cmdName
