{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import Gonimo.SocketAPI.Types (FromId, ToId, InvitationReply, InvitationInfo)
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
  | ReqSwitchFamily  !DeviceId !FamilyId


  | ReqCreateFamily
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

  | ReqCreateChannel !FromId !ToId
  | ReqSendMessage !FromId !ToId !Secret !Text
  deriving (Generic, Ord, Eq)

-- | Constructors starting with "Res" are responses to requests.
--   Constructors starting with Event happen without any request.
data ServerResponse
  = ResAuthenticated

  | ResMadeDevice !Client.AuthData
  | ResGotDeviceInfo !DeviceId !Client.DeviceInfo
  | ResSetDeviceType !DeviceId
  | ResSwitchedFamily !DeviceId !FamilyId


  | ResCreatedFamily !FamilyId
  | ResGotFamily !FamilyId !Family
  | ResGotFamilyMembers !FamilyId !([AccountId])
  | ResGotOnlineDevices !FamilyId ![(DeviceId, DeviceType)]
  | ResSavedBabyName

  | ResCreatedInvitation !(InvitationId, Invitation)
  | ResSentInvitation
  | ResClaimedInvitation !Secret !InvitationInfo
  | ResAnsweredInvitation !Secret !InvitationReply !(Maybe FamilyId)

  | ResSubscribed -- Pretty dumb response, but we don't need more information on the client side right now.

  | ResGotFamilies !AccountId !([FamilyId])
  | ResGotDevices !AccountId !([DeviceId])

  | ResCreatedChannel !FromId !ToId !Secret
  | ResSentMessage -- Dummy


  | ResError !ServerRequest !ServerError

  | EventSessionGotStolen
  | EventChannelRequested !FromId !Secret
  | EventMessageReceived !FromId !Secret !Text
  deriving (Generic)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions
