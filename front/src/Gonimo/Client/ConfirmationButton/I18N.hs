{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.ConfirmationButton.I18N where

import           Gonimo.I18N

data Message = Are_you_sure
             | OK
             deriving (Show, Eq)

instance I18N Message where
    i18n EN_GB Are_you_sure
            = "Are you sure ..."
    i18n DE_DE Are_you_sure
            = "Bist du sicher ..."
    i18n EN_GB OK
            = "OK"
    i18n DE_DE OK
            = "OK"

