{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Config where

import Data.Text (Text)
import Data.Monoid

gonimoBackServer :: Text
gonimoBackServer = "localhost:8081"

gonimoBackWSURL :: Text
gonimoBackWSURL = "wss://" <> gonimoBackServer
