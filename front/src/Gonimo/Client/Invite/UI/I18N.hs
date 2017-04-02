{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Invite.UI.I18N where

import           Data.String
import           Text.Printf
#ifdef DE
import           Panic
#enif

data Message = Invite_More_Devices
             | Invite_More_Devices
             | Done
             | Sent_WhatsApp
             | Sent_Telegram
             | Sent_Copy
             | Sent_Refresh
             | Sent_Email
             | IsString familyName => To_your_family familyName
             | Open_invitation_link_on_another_device_and_press_Accept
             | Make_the_other_device_a_parent_station
             | Copy_link_to_clipboard
             | Generate_new_link
             | IsString email => Email_successfully_sent_to email
             | Nobody
             deriving (Show, Eq)

i18n :: IsString a => Message -> a
#ifdef DE
i18n Invite_More_Devices = "Laden Sie weitere Geräte ein"
i18n Done = "Fertig"
i18n _ = sorry "not yet fully implemented"
#else
i18n Invite_More_Devices = "Invite more devices"
i18n Done = "Done"
i18n Sent_WhatsApp = "Sent with WhatsApp!"
i18n Sent_Telegram = "Sent with Telegram!"
i18n Sent_Copy     = "Copied invitation link to clipboard!"
i18n Sent_Refresh  = "New invitation generated!"
i18n Sent_Email    = "Sent email!"
i18n (To_your_family familyName) = printf "to your family '%s'" familyName
i18n Open_invitation_link_on_another_device_and_press_Accept
                   = "Open invitation link on another device and press <Accept>"
i18n Make_the_other_device_a_parent_station
                   = "Make the other device a parent station"
i18n Copy_link_to_clipboard = "Copy link to clipboard"
i18n Generate_new_link = "Generate new link"
i18n (Email_successfully_sent_to getAddr_inv) = fromString $ printf "E-mail successfully sent to:  %s" getAddr_inv
i18n Nobody = "Nobody"
#endif
{-
src/Gonimo/Client/Family/UI.hs
35:      text "Welcome to the "
37:      text "Gonimo World!"
43:    el "h3" $ text "Create a new Family"
65:      text "Welcome to the "
67:      text "Gonimo World!"
73:    el "h3" $ text "FAMILY"
118:          text " "
120:          text " "
132:        (text "Change your family name to ...")
255:    el "h1" $ text "Create New Family"

src/Gonimo/Client/Family/RoleSelector.hs
21:        el "span" $ text "BABY"
25:        el "span" $ text "PARENT"

src/Gonimo/Client/EditStringButton.hs
42:      el "h3" $ text "EDIT NAME"

src/Gonimo/Client/MessageBox/UI.hs
58:      text "The invitation got rejected and is now invalid."
62:      text "Your device is now a family member - make it a baby station!"
71:      text "You are already a member of this family - wanna switch?"
72:      switch' <- buttonAttr ("class" =: "btn btn-block") $ text "Switch Family"
76:      text "Invitations are only valid once!"
77:      elClass "span" "hidden-xs" $ text " (security and stuff, you know ...)"
81:      text "Some other device opened this invitation already!"
82:      elClass "span" "hidden-xs" $ text " (security and stuff, you know ...)"
115:  (e, _) <- elClass' "span" "close" $ text "x"

src/Gonimo/Client/Parent/UI.hs
95:            makeClickable . elAttr' "div" (addBtnAttrs "device-add") $ text " Add Device"
122:    el "h3" $ text "Really stop parent station?"
123:    el "p" $ text "All open streams will be disconnected!"

src/Gonimo/Client/App/UI.hs
85:        elClass "div" "container" $ text "Loading, stay tight..."

src/Gonimo/Client/AcceptInvitation/UI.hs
55:      el "h1" $ text "Family Invitation"
59:          el "td" $ text "Family Name:"
62:          el "td" $ text "Inviting Device:"
66:            el "td" $ text "Inviting User:"
73:          text "Decline "
76:          text "Accept "
77:          elClass "span" "hidden-xs" $ text "this generous offer "

src/Gonimo/Client/DeviceList/UI.hs
121:                conC <- makeClickable . elAttr' "div" (addBtnAttrs "connect") $ text "Connect"
122:                streamC <- makeClickable . elAttr' "div" (addBtnAttrs "stream") $ text "Stream"
123:                discC <- makeClickable . elAttr' "div" (addBtnAttrs "disconnect") $ text "Disconnect"
132:                                              $ text "Rename"
134:                              (text "Change device name to ...")
138:                                                    $ text "Remove"

src/Gonimo/Client/Baby/UI.hs
84:        startClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Start"
121:              el "h3" $ text "Really stop baby monitor?"
122:              el "p" $ text "All connected devices will be disconnected!"
132:                      =<< (makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Stop")
220:    myCheckBox (addBtnAttrs "autostart ") (baby'^.autoStartEnabled) . el "span" $ text "Autostart"
227:    elClass "span" "baby-form" $ text "Adjust camera for"
232:        text " "
237:      (text "Add new baby name ...")

src/Gonimo/Client/ConfirmationButton.hs
53:      el "h1" $ text "Are you sure ..."
61:      yesClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Ok"
-}
