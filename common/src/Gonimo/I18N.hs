module Gonimo.I18N where

import Data.Text
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToJSON, genericToEncoding)
import GHC.Generics

data Locale = DE_DE
            | EN_GB
            deriving (Show, Eq, Generic)

instance FromJSON Locale
instance ToJSON Locale where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

class I18N msg where
  {-# MINIMAL i18n #-}
  i18n :: Locale -> msg -> Text
