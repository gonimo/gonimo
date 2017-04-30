{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Baby.UI.I18N where

import           Gonimo.I18N

data Message = Start
             | Stop
             | Baby
             | Disable
             | Good_Night
             | Really_stop_baby_monitor
             | All_connected_devices_will_be_disconnected
             | Standard_Setting
             | Autostart
             | Adjust_camera_for
             | Add_new_baby_name
             deriving (Show, Eq)

instance I18N Message where
    i18n EN Start      = "START"
    i18n EN Stop       = "STOP"
    i18n EN Baby       = "BABY"
    i18n EN Disable    = "DISABLE"
    i18n EN Good_Night = "Good Night"
    i18n EN Really_stop_baby_monitor
                       = "Really stop baby monitor?"
    i18n EN All_connected_devices_will_be_disconnected
                       = "All connected devices will be disconnected!"
    i18n EN Standard_Setting = "Standard Setting"
    i18n EN Autostart = "AUTOSTART"
    i18n EN Adjust_camera_for = "Adjust camera for"
    i18n EN Add_new_baby_name = "Add new baby name ..."
    i18n _ _ = error "not yet fully implemented"
