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
    i18n EN_GB Invite_More_Devices = "Invite more devices"
    i18n EN_GB Done = "DONE"
    i18n EN_GB Sent_WhatsApp = "Sent with WhatsApp!"
    i18n EN_GB Sent_Telegram = "Sent with Telegram!"
    i18n EN_GB Sent_Copy     = "Copied invitation link to clipboard!"
    i18n EN_GB Sent_Refresh  = "New invitation generated!"
    i18n EN_GB Sent_Email    = "Sent email!"
    i18n EN_GB (To_your_family familyName) = "to your family '"<> familyName<>"'."
    i18n EN_GB Open_invitation_link_on_another_device_and_press_Accept
                          = "Open invitation link on another device and press <Accept>"
    i18n EN_GB Make_the_other_device_a_parent_station
                          = "Make the other device a parent station"
    i18n EN_GB Copy_link_to_clipboard = "Copy link to clipboard"
    i18n EN_GB Generate_new_link = "Generate new link"
    i18n EN_GB (Email_successfully_sent_to getAddr_inv) = "E-mail successfully sent to: "<>getAddr_inv
    i18n EN_GB Nobody = "Nobody"

    i18n DE_DE Invite_More_Devices = "Laden Sie weitere Geräte ein"
    i18n DE_DE Done = "FERTIG"
    i18n DE_DE msg = i18n EN_GB msg
    
