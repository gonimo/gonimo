{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.UI.Dialogs.Invite.I18N where

import           Gonimo.I18N

data Message = Invitation_Code
             | Cancel
             | Loading
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Invitation_Code
          = "Invitation Code"
  i18n DE_DE Invitation_Code
          = "Einladungscode"
  i18n EN_GB Cancel
          = "Cancel"
  i18n DE_DE Cancel
          = "Abbrechen"
  i18n EN_GB Loading
          = "Loading ..."
  i18n DE_DE Loading
          = "Lade ..."
