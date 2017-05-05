{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Family.UI.I18N where

import           Gonimo.I18N
import           Data.Text (Text)
import           Data.Monoid ((<>))

data Message = Welcome_to_the
             | Gonimo_World
             | Create_a_new_Family
             | Create_New_Family
             | Family_name
             | FamilyText
             | Add_Device
             | Really_leave_family Text
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Welcome_to_the
          = "Welcome to the "
  i18n DE_DE Welcome_to_the
          = "Willkommen in der "
  i18n EN_GB Gonimo_World
          = "Gonimo World!"
  i18n DE_DE Gonimo_World
          = "Gonimo Welt!"
  i18n EN_GB Create_a_new_Family
          = "Create a new Family"
  i18n DE_DE Create_a_new_Family
          = "Erstelle eine neue Familie"
  i18n EN_GB Family_name
          = "FAMILY NAME"
  i18n DE_DE Family_name
          = "FAMILIEN NAME"
  i18n EN_GB Create_New_Family -- do we need this with Create_a_new_Family already existing
          = "Create New Family"
  i18n DE_DE Create_New_Family -- do we need this with Create_a_new_Family already existing
          = "Erstelle eine neue Familie"
  i18n EN_GB FamilyText
          = "FAMILY"
  i18n DE_DE FamilyText
          = "FAMILIE"
  i18n EN_GB Add_Device
          = " ADD DEVICE"
  i18n DE_DE Add_Device
          = " GERÄT HINZUFÜGEN"
  i18n EN_GB (Really_leave_family cFamilyName)
          = "Really leave family '" <> cFamilyName <> "'?"
  i18n DE_DE (Really_leave_family cFamilyName)
          = "Willst du wirklich die Familie '" <> cFamilyName <> "' verlassen?"

