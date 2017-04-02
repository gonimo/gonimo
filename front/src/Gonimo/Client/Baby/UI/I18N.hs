{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Baby.UI.I18N where

import           Data.String
import           Text.Printf
#ifdef DE
import           Panic
#enif

data Message = Start
             | Stop
             | Good_Night
             | Really_stop_baby_monitor
             | All_connected_devices_will_be_disconnected
             | Standard_Setting
             | Autostart
             | Adjust_camera_for
             | Add_new_baby_name
             deriving (Show, Eq)

i18n :: IsString a => Message -> a
#ifdef DE
i18n _ = sorry "not yet fully implemented"
#else
i18n Start      = "Start"
i18n Stop       = "Stop"
i18n Good_Night = "Good Night"
i18n Really_stop_baby_monitor
                = "Really stop baby monitor?"
i18n All_connected_devices_will_be_disconnected
                = "All connected devices will be disconnected!"
i18n Standard_Setting
                = "Standard Setting"
i18n Autostart = "Autostart"
i18n Adjust_camera_for = "Adjust camera for"
i18n Add_new_baby_name = "Add new baby name ..."
#endif
