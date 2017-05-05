{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.AcceptInvitation.UI.I18N where

import           Gonimo.I18N

data Message = Family_Invitation
             | Family_Name
             | Inviting_Device
             | Inviting_User
             | Decline
             | Accept
             | This_generous_offer
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Family_Invitation = "Family Invitation"
  i18n DE_DE Family_Invitation = "Familieneinladung"
  i18n EN_GB Family_Name = "Family Name:"
  i18n DE_DE Family_Name = "Familienname:"
  i18n EN_GB Inviting_Device = "Inviting Device:"
  i18n DE_DE Inviting_Device = "Einladendes Gerät:"
  i18n EN_GB Inviting_User = "Inviting User:"
  i18n DE_DE Inviting_User = "Einladung von:"
  i18n EN_GB Decline = "Decline"
  i18n DE_DE Decline = "Ablehnen"
  i18n EN_GB Accept = "Accept"
  i18n DE_DE Accept = "Akzeptiere"
  i18n EN_GB This_generous_offer = " this generous offer "
  i18n DE_DE This_generous_offer = " dieses großzügige Angebot "
