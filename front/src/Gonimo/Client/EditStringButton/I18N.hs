{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.EditStringButton.I18N where

import           Gonimo.I18N


data Message = Change_family_name_to
             | Change_device_name_to
             | Edit_Name
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Change_family_name_to
          = "Change family name to ... "
  i18n DE_DE Change_family_name_to
          = "Familie umbenennen auf ..."
  i18n EN_GB Change_device_name_to
          = "Change device name to ..."
  i18n DE_DE Change_device_name_to
          = "Gerät umbenennen auf ..."
  i18n EN_GB Edit_Name
          = "EDIT NAME"
  i18n DE_DE Edit_Name
          = "NAMEN ÄNDERN"
