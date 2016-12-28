{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import GHC.Generics (Generic)

type MessageId = Int
data RequestBody = ReqMakeDevice (Maybe Text) deriving (Generic)

data ResponseBody = ResMakeDevice !Client.AuthData deriving (Generic)

data APIRequest = APIRequest !MessageId !RequestBody deriving (Generic)

data APIResponse = APIResponse !MessageId !ResponseBody deriving (Generic)

instance FromJSON APIResponse
instance ToJSON APIResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON APIRequest
instance ToJSON APIRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RequestBody
instance ToJSON RequestBody where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ResponseBody
instance ToJSON ResponseBody where
  toEncoding = genericToEncoding defaultOptions
