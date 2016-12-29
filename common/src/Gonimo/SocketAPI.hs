{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL

type MessageId = Int
data ServerRequest = ReqMakeDevice (Maybe Text) deriving (Generic)

data ServerResponse = ResMakeDevice !Client.AuthData deriving (Generic)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions
