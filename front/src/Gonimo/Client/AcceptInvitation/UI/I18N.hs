{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.AcceptInvitation.UI.I18N where

import           Data.String
import           Text.Printf
#ifdef DE
import           Panic
#enif

data Message = Start
             deriving (Show, Eq)

i18n :: IsString a => Message -> a
#ifdef DE
i18n _ = sorry "not yet fully implemented"
#else
i18n Family_Invitation = "Family Invitation"
i18n Family_Name = "Family Name:"
i18n Inviting_Device = "Inviting Device:"
i18n Inviting_User = "Inviting User:"
i18n Decline = "Decline"
i18n Accept = "Accept"
i18n This_generous_offer = "this generous offer"
#endif
