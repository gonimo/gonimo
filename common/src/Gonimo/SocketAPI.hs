{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import GHC.Generics (Generic)
import Gonimo.Server.Error (ServerError)
import Gonimo.Db.Entities (FamilyId, InvitationId, Invitation, DeviceId)
import Gonimo.Types (AuthToken)

type MessageId = Int
data ServerRequest
  = ReqMakeDevice !(Maybe Text)
  | ReqAuthenticate !AuthToken
  | ReqCreateFamily
  | ReqCreateInvitation !FamilyId
  | ReqSetSubscriptions !([ServerRequest])
  | ReqGetDeviceInfo !DeviceId
  | ReqGetFamily !FamilyId
  | ReqGetOnlineDevices !FamilyId
  | ReqCreateChannel !FromId !ToId
  | ReqSendMessage !FromId !ToId !Secret !Text
  deriving (Generic, Ord, Eq)

-- | Constructors starting with "Res" are responses to requests.
--   Constructors starting with Event happen without any request.
data ServerResponse = ResMadeDevice !Client.AuthData
  | ResAuthenticated
  | ResCreatedFamily !FamilyId
  | ResCreatedInvitation !(InvitationId, Invitation)
  | ResSubscribed -- Pretty dumb response, but we don't need more information on the client side right now.
  | ResCreatedChannel !FromId !ToId !Secret
  | EventSessionGotStolen
  | EventChannelRequested !FromId !Secret
  | EventMessageReceived !FromId !Secret !Text
  | ResError !ServerRequest !ServerError
  deriving (Generic)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions
