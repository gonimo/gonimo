{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.MessageBox.UI.I18N where

import           Gonimo.I18N

data Message = The_invitation_got_rejected_and_is_now_invalid
             | Your_device_is_now_a_family_member_make_it_a_baby_station
             | You_are_already_a_member_of_this_family_wanna_switch
             | Switch_Family
             | Invitations_are_only_valid_once
             | Security_and_stuff_you_know
             | Some_other_device_opened_this_invitation_already
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB The_invitation_got_rejected_and_is_now_invalid
          = "The invitation got rejected and is now invalid."
  i18n EN_GB Your_device_is_now_a_family_member_make_it_a_baby_station
          = "Your device is now a family member - make it a baby station!"
  i18n EN_GB You_are_already_a_member_of_this_family_wanna_switch
          = "You are already a member of this family - wanna switch?"
  i18n EN_GB Switch_Family
          = "Switch Family"
  i18n EN_GB Invitations_are_only_valid_once
          = "Invitations are only valid once!"
  i18n EN_GB Security_and_stuff_you_know
          = " (security and stuff, you know ...)"
  i18n EN_GB Some_other_device_opened_this_invitation_already
          = "Some other device opened this invitation already!"

  i18n DE_DE msg = i18n EN_GB msg
