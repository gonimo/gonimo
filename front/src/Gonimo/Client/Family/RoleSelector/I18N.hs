module Gonimo.Client.Family.RoleSelector.I18N where

import           Gonimo.I18N


data Message = Baby
             | Parent
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Baby
          = "BABY"
  i18n EN_GB Parent
          = "PARENT"
  i18n DE_DE msg = i18n EN_GB msg


