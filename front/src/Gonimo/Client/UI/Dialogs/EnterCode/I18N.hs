{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.UI.Dialogs.EnterCode.I18N where

import           Gonimo.I18N

data Message = Accept_Invitation
             | Cancel
             | Only_valid_for
             | Seconds
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Accept_Invitation
          = "Accept Invitation"
  i18n DE_DE Accept_Invitation
          = "Einladung akzeptieren"
  i18n EN_GB Cancel
          = "Cancel"
  i18n DE_DE Cancel
          = "Abbrechen"
  i18n EN_GB Only_valid_for
          = "Only_valid_for"
  i18n DE_DE Only_valid_for
          = "Nur"
  i18n EN_GB Seconds
          = "seconds"
  i18n DE_DE Seconds
          = "Sekunden gültig"
