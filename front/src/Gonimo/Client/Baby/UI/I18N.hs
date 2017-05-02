{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Baby.UI.I18N where

import           Gonimo.I18N

data Message = Start
             | Stop
             | BabyText
             | Disable
             | Good_Night
             | Really_stop_baby_monitor
             | All_connected_devices_will_be_disconnected
             | Standard_Setting
             | Autostart
             | Adjust_camera_for
             | Add_new_baby_name
             | Please_click_on_the_lock_symbol
             | Please_click_on_the_lock_or_the_camera_symbol
             | Error_so_sad
             | Can_not_access_your_camera_or_microphone
             | Obviously_those_are_needed_for_a_baby_monitor
             | Autostart_active
             | Baby_monitor_will_start_automatically
             | For_video_to_work_please_do_not_switch_off_the_screen
             | Alternatively_if_all_you_need_is_audio_please_disable_the_camera
             | Try_Again
             deriving (Show, Eq)

instance I18N Message where
    i18n EN_GB Start
            = "START"
    i18n DE_DE Start
            = "STARTEN"
    i18n EN_GB Stop
            = "STOP"
    i18n DE_DE Stop
            = "STOPP"
    i18n EN_GB BabyText
            = "BABY"
    i18n DE_DE BabyText
            = "BABY"
    i18n EN_GB Disable
            = "DISABLE"
    i18n DE_DE Disable
            = "Deaktivieren"
    i18n EN_GB Good_Night
            = "Good Night"
    i18n DE_DE Good_Night
            = "Gute Nacht"
    i18n EN_GB Really_stop_baby_monitor
            = "Really stop baby monitor?"
    i18n DE_DE Really_stop_baby_monitor
            = "Babyphon wirklich beenden?"
    i18n EN_GB All_connected_devices_will_be_disconnected
            = "All connected devices will be disconnected!"
    i18n DE_DE All_connected_devices_will_be_disconnected
            = "Alle verbundenen Geräte werden getrennt!"
    i18n EN_GB Standard_Setting
            = "Standard Setting"
    i18n DE_DE Standard_Setting
            = "Standard Einstellung"
    i18n EN_GB Autostart
            = "AUTOSTART"
    i18n DE_DE Autostart
            = "AUTOSTART"
    i18n EN_GB Adjust_camera_for
            = "Adjust camera for"
    i18n DE_DE Adjust_camera_for
            = "Kamera einrichten für"
    i18n EN_GB Add_new_baby_name
            = "Add new baby name ..."
    i18n DE_DE Add_new_baby_name
            = "Neuen Babynamen hinzufügen"
    i18n EN_GB Please_click_on_the_lock_symbol
            = "Please click on the lock symbol in the browser address bar for accessing the settings."
    i18n DE_DE Please_click_on_the_lock_symbol
            = "Bitte klicke auf das Schlosssymbol in der Adresszeile um die Einstellungen zu erreichen."
    i18n EN_GB Please_click_on_the_lock_or_the_camera_symbol
            = "Please click on the lock or the camera symbol in the browser address bar for accessing the settings."
    i18n DE_DE Please_click_on_the_lock_or_the_camera_symbol
            = "Bitte klicke auf das Schlosssymbol oder das Kamerasymbol in der Adresszeile um diese Einstellungen zu erreichen."
    i18n EN_GB Error_so_sad
            = "Error - so sad!"
    i18n DE_DE Error_so_sad
            = "Fehler, kein Zugriff!"
    i18n EN_GB Can_not_access_your_camera_or_microphone
            = "Can not access your camera or microphone!"
    i18n DE_DE Can_not_access_your_camera_or_microphone
            = "Ich kann nicht auf deine Kamera oder dein Mikrofon zugreifen!"
    i18n EN_GB Obviously_those_are_needed_for_a_baby_monitor
            = "Obviously those are needed for a baby monitor. Please check your browser settings whether gonimo is allowed access."
    i18n DE_DE Obviously_those_are_needed_for_a_baby_monitor
            = "Diese werden offensichtlich für ein Babyphon benötigt. Bitte überprüfe in deinen Browsereinstellungen ob Gonimo Zugriff gewährt wird."
    i18n EN_GB Autostart_active
            = "Autostart active ..."
    i18n DE_DE Autostart_active
            = "Autostart aktiv ..."
    i18n EN_GB Baby_monitor_will_start_automatically
            = "Baby monitor will start automatically, when you load the app next time."
    i18n DE_DE Baby_monitor_will_start_automatically
            = "Die Babystation wird automatisch starten, wenn du die App das nächste mal lädst."
    i18n EN_GB For_video_to_work_please_do_not_switch_off_the_screen
            = "For video to work, please do not switch off the screen!"
    i18n DE_DE For_video_to_work_please_do_not_switch_off_the_screen
            = "Damit Video funktioniert, lasse bitte den Bildschirm laufen!"
    i18n EN_GB Alternatively_if_all_you_need_is_audio_please_disable_the_camera
            = "Alternatively, if all you need is audio, please disable the camera."
    i18n DE_DE Alternatively_if_all_you_need_is_audio_please_disable_the_camera
            = "Andernfalls, wenn du nur Ton brauchst, schalte bitte die Kamera aus."
    i18n EN_GB Try_Again
            = "TRY AGAIN"
    i18n DE_DE Try_Again
            = "ERNEUT VERSUCHEN"
