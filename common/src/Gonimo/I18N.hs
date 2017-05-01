module Gonimo.I18N where

import Data.Text

data Locale = DE_DE
            | EN_GB
            deriving (Show, Eq)

class I18N msg where
  {-# MINIMAL i18n #-}
  i18n :: Locale -> msg -> Text
