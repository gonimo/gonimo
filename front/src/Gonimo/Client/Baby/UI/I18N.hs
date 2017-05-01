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
    i18n EN_GB Start      = "START"
    i18n EN_GB Stop       = "STOP"
    i18n EN_GB Baby       = "BABY"
    i18n EN_GB Disable    = "DISABLE"
    i18n EN_GB Good_Night = "Good Night"
    i18n EN_GB Really_stop_baby_monitor
                       = "Really stop baby monitor?"
    i18n EN_GB All_connected_devices_will_be_disconnected
                       = "All connected devices will be disconnected!"
    i18n EN_GB Standard_Setting = "Standard Setting"
    i18n EN_GB Autostart = "AUTOSTART"
    i18n EN_GB Adjust_camera_for = "Adjust camera for"
    i18n EN_GB Add_new_baby_name = "Add new baby name ..."
    i18n _ _ = error "not yet fully implemented"
