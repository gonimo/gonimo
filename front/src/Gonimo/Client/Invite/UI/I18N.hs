{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Invite.UI.I18N where

import           Gonimo.I18N
import           Data.Text
import           Data.Monoid

data Message = Invite_More_Devices
             | Done
             | Email
             | Share
             | Invitation_Link
             | Sent_WhatsApp -- No longer in use
             | Sent_Telegram -- No longer in use
             | Sent_Copy
             | Sent_Refresh
             | Sent_Email
             | Sent_Share
             | To_your_family Text
             | Open_invitation_link_on_another_device_and_press_Accept
             | Make_the_other_device_a_parent_station
             | Copy_link_to_clipboard
             | Generate_new_link
             | Email_successfully_sent_to Text
             | Nobody
             | SEND
             deriving (Show, Eq)


instance I18N Message where
    i18n EN_GB SEND = "SEND"
    i18n DE_DE SEND = "SENDEN"
    i18n EN_GB Share = "Share"
    i18n DE_DE Share = "Teilen"
    i18n EN_GB Invite_More_Devices = "Invite more devices"
    i18n DE_DE Invite_More_Devices = "Lade weitere Geräte ein"
    i18n EN_GB Done = "DONE"
    i18n DE_DE Done = "FERTIG"
    i18n EN_GB Email = "EMAIL"
    i18n DE_DE Email = "EMAIL"
    i18n EN_GB Invitation_Link = "INVITATION LINK"
    i18n DE_DE Invitation_Link = "EINLADUNGSLINK"
    i18n EN_GB Sent_WhatsApp = "Sent with WhatsApp!"
    i18n DE_DE Sent_WhatsApp = "Mit WhatsApp versendet!"
    i18n EN_GB Sent_Telegram = "Sent with Telegram!"
    i18n DE_DE Sent_Telegram = "Mit Telegram versendet!"
    i18n EN_GB Sent_Copy     = "Copied invitation link to clipboard!"
    i18n DE_DE Sent_Copy     = "Einladung in die Zwischenablage kopiert!"
    i18n EN_GB Sent_Refresh  = "New invitation generated!"
    i18n DE_DE Sent_Refresh  = "Neue Einladung generiert!"
    i18n EN_GB Sent_Email    = "Sent email!"
    i18n DE_DE Sent_Email    = "Email versendet!"
    i18n EN_GB Sent_Share    = "Invitation link shared!"
    i18n DE_DE Sent_Share    = "Einladung geteilt!"
    i18n EN_GB (To_your_family familyName) = "to your family '"<> familyName<>"'."
    i18n DE_DE (To_your_family familyName) = "in deine Familie '"<> familyName<>"' ein."
    i18n EN_GB Open_invitation_link_on_another_device_and_press_Accept
                          = "Open invitation link on another device and press <Accept>"
    i18n DE_DE Open_invitation_link_on_another_device_and_press_Accept
                          = ""
    i18n EN_GB Make_the_other_device_a_parent_station
                          = "Make the other device a parent station"
    i18n DE_DE Make_the_other_device_a_parent_station
                          = ""
    i18n EN_GB Copy_link_to_clipboard = "Copy link to clipboard"
    i18n DE_DE Copy_link_to_clipboard = "Kopiere Link in die Zwischenablage"
    i18n EN_GB Generate_new_link = "Generate new link"
    i18n DE_DE Generate_new_link = "Generiere neuen Link"
    i18n EN_GB (Email_successfully_sent_to getAddr_inv) = "E-mail successfully sent to: "<>getAddr_inv
    i18n DE_DE (Email_successfully_sent_to getAddr_inv) = "E-mail erfolgreich gesendet an: "<> getAddr_inv
    i18n EN_GB Nobody = "Nobody"
    i18n DE_DE Nobody = "Niemand"
