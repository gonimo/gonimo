{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Control.Lens
import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import Gonimo.SocketAPI.Types (FromId, ToId, InvitationReply, InvitationInfo, Message)
import GHC.Generics (Generic)
import Gonimo.Server.Error (ServerError)
import Gonimo.Db.Entities (FamilyId, InvitationId, Invitation, DeviceId, AccountId, Family)
import Gonimo.Types (AuthToken, Secret, DeviceType)

type MessageId = Int
data ServerRequest
  = ReqAuthenticate !AuthToken

  | ReqMakeDevice !(Maybe Text)
  | ReqGetDeviceInfo !DeviceId
  | ReqSetDeviceType !DeviceId !DeviceType
  | ReqSetDeviceName !DeviceId !Text
  | ReqSwitchFamily  !DeviceId !FamilyId

  | ReqCreateFamily
  | ReqSetFamilyName !FamilyId !Text
  | ReqGetFamily !FamilyId
  | ReqGetFamilyMembers !FamilyId
  | ReqGetOnlineDevices !FamilyId
  | ReqSaveBabyName !FamilyId !Text

  | ReqCreateInvitation !FamilyId
  | ReqSendInvitation !Client.SendInvitation
  | ReqClaimInvitation !Secret
  | ReqAnswerInvitation !Secret !InvitationReply

  | ReqSetSubscriptions !([ServerRequest])

  | ReqGetFamilies !AccountId
  | ReqGetDevices !AccountId
  | ReqLeaveFamily !AccountId !FamilyId

  | ReqCreateChannel !FromId !ToId
  | ReqSendMessage !FromId !ToId !Secret Message
  deriving (Generic, Ord, Eq, Show)

-- | Constructors starting with "Res" are responses to requests.
--   Constructors starting with Event happen without any request.
data ServerResponse
  = ResAuthenticated

  | ResMadeDevice !Client.AuthData
  | ResGotDeviceInfo !DeviceId !Client.DeviceInfo
  | ResSetDeviceType !DeviceId
  | ResSetDeviceName !DeviceId
  | ResSwitchedFamily !DeviceId !FamilyId


  | ResCreatedFamily !FamilyId
  | ResSetFamilyName !FamilyId
  | ResGotFamily !FamilyId !Family
  | ResGotFamilyMembers !FamilyId !([AccountId])
  | ResGotOnlineDevices !FamilyId ![(DeviceId, DeviceType)]
  | ResSavedBabyName

  | ResCreatedInvitation !(InvitationId, Invitation)
  | ResSentInvitation !Client.SendInvitation
  | ResClaimedInvitation !Secret !InvitationInfo
  | ResAnsweredInvitation !Secret !InvitationReply !(Maybe FamilyId)

  | ResSubscribed -- Pretty dumb response, but we don't need more information on the client side right now.

  | ResGotFamilies !AccountId !([FamilyId])
  | ResGotDevices !AccountId !([DeviceId])
  | ResLeftFamily !AccountId !FamilyId

  | ResCreatedChannel !FromId !ToId !Secret
  | ResSentMessage -- Dummy


  | ResError !ServerRequest !ServerError

  | EventSessionGotStolen
  | EventChannelRequested !FromId !Secret
  | EventMessageReceived !FromId !Secret Message
  deriving (Generic, Show)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions

makeClassyPrisms ''ServerRequest
makeClassyPrisms ''ServerResponse
