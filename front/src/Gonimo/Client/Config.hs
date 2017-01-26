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
