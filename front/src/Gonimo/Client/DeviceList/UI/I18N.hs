{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.DeviceList.UI.I18N where

import           Gonimo.I18N
import           Data.Text (Text)
import           Data.Monoid ((<>))

data Message = Really_leave_your_current_family
             | Do_you_really_want_to_remove_device Text
             | Rename
             | Remove
             | Connect
             | Stream
             | Disconnect
             | Last_Seen Text
             | Thats_you
             deriving (Show, Eq)

instance I18N Message where
    i18n EN_GB Really_leave_your_current_family
            = "This is you!\nReally leave your current family?"
    i18n DE_DE Really_leave_your_current_family
            = "Das bist du!\nWillst du wirklich diese Familie verlassen?"
    i18n EN_GB (Do_you_really_want_to_remove_device devName)
            = "Do you really want to remove device '" <> devName <> "' from the family?"
    i18n DE_DE (Do_you_really_want_to_remove_device devName)
            = "Willst du wirklich dieses Gerät '" <> devName <> "' aus deiner Familie entfernen?"
    i18n EN_GB Rename
            = "RENAME"
    i18n DE_DE Rename
            = "UMBENENNEN"
    i18n EN_GB Remove
            = "REMOVE"
    i18n DE_DE Remove
            = "ENTFERNEN"
    i18n EN_GB Connect
            = "Connect"
    i18n DE_DE Connect
            = "VERBINDEN"
    i18n EN_GB Stream
            = "STREAM"
    i18n DE_DE Stream
            = "STREAM"
    i18n EN_GB Disconnect
            = "Disconnect"
    i18n DE_DE Disconnect
            = "TRENNEN"
    i18n EN_GB (Last_Seen seen) -- TODO localization of time format
            = "Last Seen: " <> seen
    i18n DE_DE (Last_Seen seen) -- TODO localization of time format
            = "Zuletzt gesehen: " <> seen
    i18n EN_GB Thats_you
            = "that's you"
    i18n DE_DE Thats_you
            = "Das bist du"

