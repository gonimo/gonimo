{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Auth.I18N where

import           Gonimo.I18N

data Message = No_Internet_Connection
             | Reconnecting
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB No_Internet_Connection
          = "No Internet Connection?"
  i18n DE_DE No_Internet_Connection
          = "Keine Internetverbindung?"
  i18n EN_GB Reconnecting
          = "Reconnecting "
  i18n DE_DE Reconnecting
          = "Verbinde "
