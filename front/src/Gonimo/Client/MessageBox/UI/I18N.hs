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
             | Invitation_rejected
             | Invitation_accepted
             | Already_a_member_of_this_family
             | Invitation_not_found
             | Invitation_already_claimed
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB The_invitation_got_rejected_and_is_now_invalid
          = "The invitation got rejected and is now invalid."
  i18n DE_DE The_invitation_got_rejected_and_is_now_invalid
          = "Die Einladung wurde abgelehnt und ist jetzt ungültig."
  i18n EN_GB Your_device_is_now_a_family_member_make_it_a_baby_station
          = "Your device is now a family member - make it a baby station!"
  i18n DE_DE Your_device_is_now_a_family_member_make_it_a_baby_station
          = "Dein Gerät ist jetzt ein Familienmitglied, mach es zur Babystation!"
  i18n EN_GB You_are_already_a_member_of_this_family_wanna_switch
          = "You are already a member of this family - wanna switch?"
  i18n DE_DE You_are_already_a_member_of_this_family_wanna_switch
          = "Du bist bereits Familienmitglied, willst du zu ihr wechseln?"
  i18n EN_GB Switch_Family
          = "Switch Family"
  i18n DE_DE Switch_Family
          = "Wechsle Familie"
  i18n EN_GB Invitations_are_only_valid_once
          = "Invitations are only valid once!"
  i18n DE_DE Invitations_are_only_valid_once
          = "Einladungen sind nur einmal gültig!"
  i18n EN_GB Security_and_stuff_you_know
          = " (security and stuff, you know ...)"
  i18n DE_DE Security_and_stuff_you_know
          = " (Sicherheitszeugs und so ...)"
  i18n EN_GB Some_other_device_opened_this_invitation_already
          = "Some other device opened this invitation already!"
  i18n DE_DE Some_other_device_opened_this_invitation_already
          = "Diese Einladung wurde bereits auf einem anderen Gerät geöffnet!"
  i18n EN_GB Invitation_rejected
          = "Invitation rejected!"
  i18n DE_DE Invitation_rejected
          = "Einladung abgelehnt!"
  i18n EN_GB Invitation_accepted
          = "Invitation accepted!"
  i18n DE_DE Invitation_accepted
          = "Einladung akzeptiert!"
  i18n EN_GB Already_a_member_of_this_family
          = "Already a member of this family!"
  i18n DE_DE Already_a_member_of_this_family
          = "Bereits Familienmitglied!"
  i18n EN_GB Invitation_not_found
          = "Invitation not found!"
  i18n DE_DE Invitation_not_found
          = "Einladung wurde nicht gefunden!"
  i18n EN_GB Invitation_already_claimed
          = "Invitation already claimed!"
  i18n DE_DE Invitation_already_claimed
          = "Einladung bereits geöffnet!"
