{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.AcceptInvitation.UI.I18N where

import           Data.String
import           Text.Printf
#ifdef DE
import           Panic
#enif
import Gonimo.I18N


data Message = Family_Invitation
               Family_Name
               Inviting_Device
               Inviting_User
               Decline
               Accept
               This_generous_offer
             deriving (Show, Eq)

instance I18N where
  i18n EN Family_Invitation = "Family Invitation"
  i18n EN Family_Name = "Family Name:"
  i18n EN Inviting_Device = "Inviting Device:"
  i18n EN Inviting_User = "Inviting User:"
  i18n EN Decline = "Decline"
  i18n EN Accept_this_generous_offer = "Accept \nthis generous offer "

  i18n _ _ = sorry "not yet fully implemented"
