{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Config where

import Data.Text (Text)
import Data.Monoid

secure :: Bool
secure = False

prefix :: Text
prefix = if secure
         then "wss://"
         else "ws://"

gonimoBackServer :: Text
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
