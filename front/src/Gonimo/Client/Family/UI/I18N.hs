{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Family.UI.I18N where

import           Gonimo.I18N
import           Data.Text (Text)
import           Data.Monoid ((<>))

data Message = Welcome_to_the
             | Gonimo_World
             | Create_a_new_Family
             | Create_New_Family
             | FamilyText
             | Add_Device
             | Really_leave_family Text
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Welcome_to_the
          = "Welcome to the "
  i18n EN_GB Gonimo_World
          = "Gonimo World!"
  i18n EN_GB Create_a_new_Family
          = "Create a new Family"
  i18n EN_GB Create_New_Family -- do we need this with Create_a_new_Family already existing
          = "Create New Family"
  i18n EN_GB FamilyText
          = "FAMILY"
  i18n EN_GB Add_Device
          = " ADD DEVICE"
  i18n EN_GB (Really_leave_family cFamilyName)
          = "Really leave family '" <> cFamilyName <> "'?"
  i18n DE_DE msg = i18n EN_GB msg

