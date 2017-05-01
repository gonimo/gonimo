{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Invite.UI.I18N where

import           Gonimo.I18N
import           Data.Text
import           Data.Monoid

data Message = Invite_More_Devices
             | Done
             | Sent_WhatsApp
             | Sent_Telegram
             | Sent_Copy
             | Sent_Refresh
             | Sent_Email
             | To_your_family Text
             | Open_invitation_link_on_another_device_and_press_Accept
             | Make_the_other_device_a_parent_station
             | Copy_link_to_clipboard
             | Generate_new_link
             | Email_successfully_sent_to Text
             | Nobody
             deriving (Show, Eq)


instance I18N Message where
    i18n DE Invite_More_Devices = "Laden Sie weitere Geräte ein"
    i18n DE Done = "Fertig"
    i18n DE _ = error "not yet implemented"
    
    i18n EN Invite_More_Devices = "Invite more devices"
    i18n EN Done = "Done"
    i18n EN Sent_WhatsApp = "Sent with WhatsApp!"
    i18n EN Sent_Telegram = "Sent with Telegram!"
    i18n EN Sent_Copy     = "Copied invitation link to clipboard!"
    i18n EN Sent_Refresh  = "New invitation generated!"
    i18n EN Sent_Email    = "Sent email!"
    i18n EN (To_your_family familyName) = "to your family '"<> familyName<>"'."
    i18n EN Open_invitation_link_on_another_device_and_press_Accept
                          = "Open invitation link on another device and press <Accept>"
    i18n EN Make_the_other_device_a_parent_station
                          = "Make the other device a parent station"
    i18n EN Copy_link_to_clipboard = "Copy link to clipboard"
    i18n EN Generate_new_link = "Generate new link"
    i18n EN (Email_successfully_sent_to getAddr_inv) = "E-mail successfully sent to: "<>getAddr_inv
    i18n EN Nobody = "Nobody"
