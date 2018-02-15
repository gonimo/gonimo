{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Config where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe (isJust)

secure :: Bool
secure = False

prefix :: Text
prefix = if secure
         then "wss://"
         else "ws://"

httpProtocol :: Text
httpProtocol = if secure then "https://" else "http://"

gonimoFrontHost :: Maybe Text
gonimoFrontHost = if T.isInfixOf "localhost" gonimoBackServer
                  then Nothing
                  else Just $ "app" <> T.dropWhile (/= '.') gonimoBackServer

gonimoFrontPath :: Text
gonimoFrontPath = if isJust gonimoFrontHost
                  then "/"
                  else "/index.html"

gonimoBackServer :: Text
-- gonimoBackServer = "b00.alpha.gonimo.com"
gonimoBackServer = "localhost:8081"

gonimoBackWSURL :: Text
gonimoBackWSURL = prefix <> gonimoBackServer

gonimoTurnServer :: Text
gonimoTurnServer = "turn:gonimo.com:3478"


-- FIXME: Should not be hard coded ...
gonimoTurnUser :: Text
gonimoTurnUser = "gonimo"

-- FIXME: Should not be hard coded ...
gonimoTurnPassword :: Text
gonimoTurnPassword = "Aeloh5chai2eil1"

gonimoTurnCredentialType :: Text
gonimoTurnCredentialType = "password"
