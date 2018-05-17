{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Control.Lens
import           Data.Aeson.Types       (FromJSON, ToJSON (..), defaultOptions,
                                         genericToEncoding)
import           Data.Text              (Text)
import           Data.Time              (DiffTime)
import           GHC.Generics           (Generic)

import           Gonimo.Server.Error    (ServerError)
import           Gonimo.SocketAPI.Types (AccountId, DeviceId, Family, FamilyId,
                                         FromId, Invitation, InvitationCode,
                                         InvitationId, InvitationInfo,
                                         InvitationReply, InvitationSecret,
                                         Message, ToId)
import qualified Gonimo.SocketAPI.Types as Client
import           Gonimo.Types           (AuthToken, DeviceType, Secret)

type MessageId = Int

data ServerRequest
  = ReqPing -- We can't rely on WebSocket ping control messages, because the client can't see them!
  | ReqAuthenticate !AuthToken

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

    -- | Claim an invitation by a given short lived `InvitationCode`.
  | ReqClaimInvitationByCode !InvitationCode

    -- | Answer an invitation you have previously claimed.
  | ReqAnswerInvitation !Secret !InvitationReply

  | ReqGetFamilyInvitations !FamilyId
  | ReqGetInvitation !InvitationId

    -- | Create a user readable and writable code for the given invitation.
  | ReqCreateInvitationCode !InvitationId

  | ReqSetSubscriptions ![ServerRequest]

  | ReqGetFamilies !AccountId
  | ReqGetDevices !AccountId
  | ReqLeaveFamily !AccountId !FamilyId

  | ReqCreateChannel !FromId !ToId
  | ReqSendMessage !FromId !ToId !Secret Message
  deriving (Generic, Ord, Eq, Show)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions


-- | Constructors starting with "Res" are responses to requests.
--   Constructors starting with Event happen without any request.
data ServerResponse
  = ResPong
  | ResAuthenticated
  | ResMadeDevice !Client.AuthData
  | ResGotDeviceInfo !DeviceId !Client.DeviceInfo
  | ResSetDeviceType !DeviceId
  | ResSetDeviceName !DeviceId
  | ResSwitchedFamily !DeviceId !FamilyId


  | ResCreatedFamily !FamilyId
  | ResSetFamilyName !FamilyId
  | ResGotFamily !FamilyId !Family
  | ResGotFamilyMembers !FamilyId ![AccountId]
  | ResGotOnlineDevices !FamilyId ![(DeviceId, DeviceType)]
  | ResSavedBabyName

  | ResCreatedInvitation !(InvitationId, Invitation)
  | ResSentInvitation !Client.SendInvitation
  | ResClaimedInvitation !InvitationSecret !InvitationInfo
  | ResClaimedInvitationByCode !InvitationCode !InvitationSecret !InvitationInfo
  | ResAnsweredInvitation !InvitationSecret !InvitationReply !(Maybe FamilyId)
  | ResGotFamilyInvitations !FamilyId ![InvitationId]
  | ResGotInvitation !InvitationId !Invitation

    -- | Made an `InvitationId` corresponding to the given `InvitationId`
    --
    --   The code will be valid for the given time in seconds of the last parameter.
  | ResCreatedInvitationCode !InvitationId !InvitationCode !Int


  | ResSubscribed -- Pretty dumb response, but we don't need more information on the client side right now.

  | ResGotFamilies !AccountId ![FamilyId]
  | ResGotDevices !AccountId ![DeviceId]
  | ResLeftFamily !AccountId !FamilyId

  | ResCreatedChannel !FromId !ToId !Secret
  | ResSentMessage -- Dummy


  | ResError !ServerRequest !ServerError

  | EventSessionGotStolen
  | EventChannelRequested !FromId !Secret
  | EventMessageReceived !FromId !Secret Message
  deriving (Generic, Show)

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions

class AsServerRequest r where
  _ServerRequest :: Prism' r ServerRequest
  _ReqPing :: Prism' r ()
  _ReqAuthenticate :: Prism' r AuthToken
  _ReqMakeDevice :: Prism' r (Maybe Text)
  _ReqGetDeviceInfo :: Prism' r DeviceId
  _ReqSetDeviceType :: Prism' r (DeviceId, DeviceType)
  _ReqSetDeviceName :: Prism' r (DeviceId, Text)
  _ReqSwitchFamily :: Prism' r (DeviceId, FamilyId)
  _ReqCreateFamily :: Prism' r ()
  _ReqSetFamilyName :: Prism' r (FamilyId, Text)
  _ReqGetFamily :: Prism' r FamilyId
  _ReqGetFamilyMembers :: Prism' r FamilyId
  _ReqGetOnlineDevices :: Prism' r FamilyId
  _ReqSaveBabyName :: Prism' r (FamilyId, Text)
  _ReqCreateInvitation :: Prism' r FamilyId
  _ReqSendInvitation :: Prism' r Client.SendInvitation
  _ReqClaimInvitation :: Prism' r Secret
  _ReqAnswerInvitation :: Prism' r (Secret, InvitationReply)
  _ReqSetSubscriptions :: Prism' r [ServerRequest]
  _ReqGetFamilies :: Prism' r AccountId
  _ReqGetDevices :: Prism' r AccountId
  _ReqLeaveFamily :: Prism' r (AccountId, FamilyId)
  _ReqCreateChannel :: Prism' r (FromId, ToId)
  _ReqSendMessage :: Prism' r (FromId, ToId, Secret, Message)
  _ReqPing             = _ServerRequest . _ReqPing
  _ReqAuthenticate     = _ServerRequest . _ReqAuthenticate
  _ReqMakeDevice       = _ServerRequest . _ReqMakeDevice
  _ReqGetDeviceInfo    = _ServerRequest . _ReqGetDeviceInfo
  _ReqSetDeviceType    = _ServerRequest . _ReqSetDeviceType
  _ReqSetDeviceName    = _ServerRequest . _ReqSetDeviceName
  _ReqSwitchFamily     = _ServerRequest . _ReqSwitchFamily
  _ReqCreateFamily     = _ServerRequest . _ReqCreateFamily
  _ReqSetFamilyName    = _ServerRequest . _ReqSetFamilyName
  _ReqGetFamily        = _ServerRequest . _ReqGetFamily
  _ReqGetFamilyMembers = _ServerRequest . _ReqGetFamilyMembers
  _ReqGetOnlineDevices = _ServerRequest . _ReqGetOnlineDevices
  _ReqSaveBabyName     = _ServerRequest . _ReqSaveBabyName
  _ReqCreateInvitation = _ServerRequest . _ReqCreateInvitation
  _ReqSendInvitation   = _ServerRequest . _ReqSendInvitation
  _ReqClaimInvitation  = _ServerRequest . _ReqClaimInvitation
  _ReqAnswerInvitation = _ServerRequest . _ReqAnswerInvitation
  _ReqSetSubscriptions = _ServerRequest . _ReqSetSubscriptions
  _ReqGetFamilies      = _ServerRequest . _ReqGetFamilies
  _ReqGetDevices       = _ServerRequest . _ReqGetDevices
  _ReqLeaveFamily      = _ServerRequest . _ReqLeaveFamily
  _ReqCreateChannel    = _ServerRequest . _ReqCreateChannel
  _ReqSendMessage      = _ServerRequest . _ReqSendMessage

instance AsServerRequest ServerRequest where
  _ServerRequest = id
  _ReqPing
    = prism
        (\ () -> ReqPing)
        (\ x
           -> case x of
                ReqPing -> Right ()
                _       -> Left x)
  _ReqAuthenticate
    = prism
        (\ x1 -> ReqAuthenticate x1)
        (\ x
           -> case x of
                ReqAuthenticate y1 -> Right y1
                _                  -> Left x)
  _ReqMakeDevice
    = prism
        (\ x1 -> ReqMakeDevice x1)
        (\ x
           -> case x of
                ReqMakeDevice y1 -> Right y1
                _                -> Left x)
  _ReqGetDeviceInfo
    = prism
        (\ x1 -> ReqGetDeviceInfo x1)
        (\ x
           -> case x of
                ReqGetDeviceInfo y1 -> Right y1
                _                   -> Left x)
  _ReqSetDeviceType
    = prism
        (\ (x1, x2) -> ReqSetDeviceType x1 x2)
        (\ x
           -> case x of
                ReqSetDeviceType y1 y2 -> Right (y1, y2)
                _                      -> Left x)
  _ReqSetDeviceName
    = prism
        (\ (x1, x2) -> ReqSetDeviceName x1 x2)
        (\ x
           -> case x of
                ReqSetDeviceName y1 y2 -> Right (y1, y2)
                _                      -> Left x)
  _ReqSwitchFamily
    = prism
        (\ (x1, x2) -> ReqSwitchFamily x1 x2)
        (\ x
           -> case x of
                ReqSwitchFamily y1 y2 -> Right (y1, y2)
                _                     -> Left x)
  _ReqCreateFamily
    = prism
        (\ () -> ReqCreateFamily)
        (\ x
           -> case x of
                ReqCreateFamily -> Right ()
                _               -> Left x)
  _ReqSetFamilyName
    = prism
        (\ (x1, x2) -> ReqSetFamilyName x1 x2)
        (\ x
           -> case x of
                ReqSetFamilyName y1 y2 -> Right (y1, y2)
                _                      -> Left x)
  _ReqGetFamily
    = prism
        (\ x1 -> ReqGetFamily x1)
        (\ x
           -> case x of
                ReqGetFamily y1 -> Right y1
                _               -> Left x)
  _ReqGetFamilyMembers
    = prism
        (\ x1 -> ReqGetFamilyMembers x1)
        (\ x
           -> case x of
                ReqGetFamilyMembers y1 -> Right y1
                _                      -> Left x)
  _ReqGetOnlineDevices
    = prism
        (\ x1 -> ReqGetOnlineDevices x1)
        (\ x
           -> case x of
                ReqGetOnlineDevices y1 -> Right y1
                _                      -> Left x)
  _ReqSaveBabyName
    = prism
        (\ (x1, x2) -> ReqSaveBabyName x1 x2)
        (\ x
           -> case x of
                ReqSaveBabyName y1 y2 -> Right (y1, y2)
                _                     -> Left x)
  _ReqCreateInvitation
    = prism
        (\ x1 -> ReqCreateInvitation x1)
        (\ x
           -> case x of
                ReqCreateInvitation y1 -> Right y1
                _                      -> Left x)
  _ReqSendInvitation
    = prism
        (\ x1 -> ReqSendInvitation x1)
        (\ x
           -> case x of
                ReqSendInvitation y1 -> Right y1
                _                    -> Left x)
  _ReqClaimInvitation
    = prism
        (\ x1 -> ReqClaimInvitation x1)
        (\ x
           -> case x of
                ReqClaimInvitation y1 -> Right y1
                _                     -> Left x)
  _ReqAnswerInvitation
    = prism
        (\ (x1, x2) -> ReqAnswerInvitation x1 x2)
        (\ x
           -> case x of
                ReqAnswerInvitation y1 y2 -> Right (y1, y2)
                _                         -> Left x)
  _ReqSetSubscriptions
    = prism
        (\ x1 -> ReqSetSubscriptions x1)
        (\ x
           -> case x of
                ReqSetSubscriptions y1 -> Right y1
                _                      -> Left x)
  _ReqGetFamilies
    = prism
        (\ x1 -> ReqGetFamilies x1)
        (\ x
           -> case x of
                ReqGetFamilies y1 -> Right y1
                _                 -> Left x)
  _ReqGetDevices
    = prism
        (\ x1 -> ReqGetDevices x1)
        (\ x
           -> case x of
                ReqGetDevices y1 -> Right y1
                _                -> Left x)
  _ReqLeaveFamily
    = prism
        (\ (x1, x2) -> ReqLeaveFamily x1 x2)
        (\ x
           -> case x of
                ReqLeaveFamily y1 y2 -> Right (y1, y2)
                _                    -> Left x)
  _ReqCreateChannel
    = prism
        (\ (x1, x2) -> ReqCreateChannel x1 x2)
        (\ x
           -> case x of
                ReqCreateChannel y1 y2 -> Right (y1, y2)
                _                      -> Left x)
  _ReqSendMessage
    = prism
        (\ (x1, x2, x3, x4)
           -> ReqSendMessage x1 x2 x3 x4)
        (\ x
           -> case x of
                ReqSendMessage y1 y2 y3 y4
                  -> Right (y1, y2, y3, y4)
                _ -> Left x)

class AsServerResponse r where
  _ServerResponse :: Prism' r ServerResponse
  _ResPong :: Prism' r ()
  _ResAuthenticated :: Prism' r ()
  _ResMadeDevice :: Prism' r Client.AuthData
  _ResGotDeviceInfo :: Prism' r (DeviceId, Client.DeviceInfo)
  _ResSetDeviceType :: Prism' r DeviceId
  _ResSetDeviceName :: Prism' r DeviceId
  _ResSwitchedFamily :: Prism' r (DeviceId, FamilyId)
  _ResCreatedFamily :: Prism' r FamilyId
  _ResSetFamilyName :: Prism' r FamilyId
  _ResGotFamily :: Prism' r (FamilyId, Family)
  _ResGotFamilyMembers :: Prism' r (FamilyId, [AccountId])
  _ResGotOnlineDevices :: Prism' r (FamilyId, [(DeviceId, DeviceType)])
  _ResSavedBabyName :: Prism' r ()
  _ResCreatedInvitation :: Prism' r (InvitationId, Invitation)
  _ResSentInvitation :: Prism' r Client.SendInvitation
  _ResClaimedInvitation :: Prism' r (Secret, InvitationInfo)
  _ResAnsweredInvitation :: Prism' r (Secret, InvitationReply, Maybe FamilyId)
  _ResSubscribed :: Prism' r ()
  _ResGotFamilies :: Prism' r (AccountId, [FamilyId])
  _ResGotDevices :: Prism' r (AccountId, [DeviceId])
  _ResLeftFamily :: Prism' r (AccountId, FamilyId)
  _ResCreatedChannel :: Prism' r (FromId, ToId, Secret)
  _ResSentMessage :: Prism' r ()
  _ResError :: Prism' r (ServerRequest, ServerError)
  _EventSessionGotStolen :: Prism' r ()
  _EventChannelRequested :: Prism' r (FromId, Secret)
  _EventMessageReceived :: Prism' r (FromId, Secret, Message)
  _ResPong               = _ServerResponse . _ResPong
  _ResAuthenticated      = _ServerResponse . _ResAuthenticated
  _ResMadeDevice         = _ServerResponse . _ResMadeDevice
  _ResGotDeviceInfo      = _ServerResponse . _ResGotDeviceInfo
  _ResSetDeviceType      = _ServerResponse . _ResSetDeviceType
  _ResSetDeviceName      = _ServerResponse . _ResSetDeviceName
  _ResSwitchedFamily     = _ServerResponse . _ResSwitchedFamily
  _ResCreatedFamily      = _ServerResponse . _ResCreatedFamily
  _ResSetFamilyName      = _ServerResponse . _ResSetFamilyName
  _ResGotFamily          = _ServerResponse . _ResGotFamily
  _ResGotFamilyMembers   = _ServerResponse . _ResGotFamilyMembers
  _ResGotOnlineDevices   = _ServerResponse . _ResGotOnlineDevices
  _ResSavedBabyName      = _ServerResponse . _ResSavedBabyName
  _ResCreatedInvitation  = _ServerResponse . _ResCreatedInvitation
  _ResSentInvitation     = _ServerResponse . _ResSentInvitation
  _ResClaimedInvitation  = _ServerResponse . _ResClaimedInvitation
  _ResAnsweredInvitation = _ServerResponse . _ResAnsweredInvitation
  _ResSubscribed         = _ServerResponse . _ResSubscribed
  _ResGotFamilies        = _ServerResponse . _ResGotFamilies
  _ResGotDevices         = _ServerResponse . _ResGotDevices
  _ResLeftFamily         = _ServerResponse . _ResLeftFamily
  _ResCreatedChannel     = _ServerResponse . _ResCreatedChannel
  _ResSentMessage        = _ServerResponse . _ResSentMessage
  _ResError              = _ServerResponse . _ResError
  _EventSessionGotStolen = _ServerResponse . _EventSessionGotStolen
  _EventChannelRequested = _ServerResponse . _EventChannelRequested
  _EventMessageReceived  = _ServerResponse . _EventMessageReceived

instance AsServerResponse ServerResponse where
  _ServerResponse = id
  _ResPong
    = prism
        (\ () -> ResPong)
        (\ x
           -> case x of
                ResPong -> Right ()
                _       -> Left x)
  _ResAuthenticated
    = prism
        (\ () -> ResAuthenticated)
        (\ x
           -> case x of
                ResAuthenticated -> Right ()
                _                -> Left x)
  _ResMadeDevice
    = prism
        (\ x1 -> ResMadeDevice x1)
        (\ x
           -> case x of
                ResMadeDevice y1 -> Right y1
                _                -> Left x)
  _ResGotDeviceInfo
    = prism
        (\ (x1, x2) -> ResGotDeviceInfo x1 x2)
        (\ x
           -> case x of
                ResGotDeviceInfo y1 y2 -> Right (y1, y2)
                _                      -> Left x)
  _ResSetDeviceType
    = prism
        (\ x1 -> ResSetDeviceType x1)
        (\ x
           -> case x of
                ResSetDeviceType y1 -> Right y1
                _                   -> Left x)
  _ResSetDeviceName
    = prism
        (\ x1 -> ResSetDeviceName x1)
        (\ x
           -> case x of
                ResSetDeviceName y1 -> Right y1
                _                   -> Left x)
  _ResSwitchedFamily
    = prism
        (\ (x1, x2) -> ResSwitchedFamily x1 x2)
        (\ x
           -> case x of
                ResSwitchedFamily y1 y2 -> Right (y1, y2)
                _                       -> Left x)
  _ResCreatedFamily
    = prism
        (\ x1 -> ResCreatedFamily x1)
        (\ x
           -> case x of
                ResCreatedFamily y1 -> Right y1
                _                   -> Left x)
  _ResSetFamilyName
    = prism
        (\ x1 -> ResSetFamilyName x1)
        (\ x
           -> case x of
                ResSetFamilyName y1 -> Right y1
                _                   -> Left x)
  _ResGotFamily
    = prism
        (\ (x1, x2) -> ResGotFamily x1 x2)
        (\ x
           -> case x of
                ResGotFamily y1 y2 -> Right (y1, y2)
                _                  -> Left x)
  _ResGotFamilyMembers
    = prism
        (\ (x1, x2) -> ResGotFamilyMembers x1 x2)
        (\ x
           -> case x of
                ResGotFamilyMembers y1 y2 -> Right (y1, y2)
                _                         -> Left x)
  _ResGotOnlineDevices
    = prism
        (\ (x1, x2) -> ResGotOnlineDevices x1 x2)
        (\ x
           -> case x of
                ResGotOnlineDevices y1 y2 -> Right (y1, y2)
                _                         -> Left x)
  _ResSavedBabyName
    = prism
        (\ () -> ResSavedBabyName)
        (\ x
           -> case x of
                ResSavedBabyName -> Right ()
                _                -> Left x)
  _ResCreatedInvitation
    = prism
        (\ x1 -> ResCreatedInvitation x1)
        (\ x
           -> case x of
                ResCreatedInvitation y1 -> Right y1
                _                       -> Left x)
  _ResSentInvitation
    = prism
        (\ x1 -> ResSentInvitation x1)
        (\ x
           -> case x of
                ResSentInvitation y1 -> Right y1
                _                    -> Left x)
  _ResClaimedInvitation
    = prism
        (\ (x1, x2) -> ResClaimedInvitation x1 x2)
        (\ x
           -> case x of
                ResClaimedInvitation y1 y2
                  -> Right (y1, y2)
                _ -> Left x)
  _ResAnsweredInvitation
    = prism
        (\ (x1, x2, x3)
           -> ResAnsweredInvitation x1 x2 x3)
        (\ x
           -> case x of
                ResAnsweredInvitation y1 y2 y3
                  -> Right (y1, y2, y3)
                _ -> Left x)
  _ResSubscribed
    = prism
        (\ () -> ResSubscribed)
        (\ x
           -> case x of
                ResSubscribed -> Right ()
                _             -> Left x)
  _ResGotFamilies
    = prism
        (\ (x1, x2) -> ResGotFamilies x1 x2)
        (\ x
           -> case x of
                ResGotFamilies y1 y2 -> Right (y1, y2)
                _                    -> Left x)
  _ResGotDevices
    = prism
        (\ (x1, x2) -> ResGotDevices x1 x2)
        (\ x
           -> case x of
                ResGotDevices y1 y2 -> Right (y1, y2)
                _                   -> Left x)
  _ResLeftFamily
    = prism
        (\ (x1, x2) -> ResLeftFamily x1 x2)
        (\ x
           -> case x of
                ResLeftFamily y1 y2 -> Right (y1, y2)
                _                   -> Left x)
  _ResCreatedChannel
    = prism
        (\ (x1, x2, x3)
           -> ResCreatedChannel x1 x2 x3)
        (\ x
           -> case x of
                ResCreatedChannel y1 y2 y3
                  -> Right (y1, y2, y3)
                _ -> Left x)
  _ResSentMessage
    = prism
        (\ () -> ResSentMessage)
        (\ x
           -> case x of
                ResSentMessage -> Right ()
                _              -> Left x)
  _ResError
    = prism
        (\ (x1, x2) -> ResError x1 x2)
        (\ x
           -> case x of
                ResError y1 y2 -> Right (y1, y2)
                _              -> Left x)
  _EventSessionGotStolen
    = prism
        (\ () -> EventSessionGotStolen)
        (\ x
           -> case x of
                EventSessionGotStolen -> Right ()
                _                     -> Left x)
  _EventChannelRequested
    = prism
        (\ (x1, x2) -> EventChannelRequested x1 x2)
        (\ x
           -> case x of
                EventChannelRequested y1 y2
                  -> Right (y1, y2)
                _ -> Left x)
  _EventMessageReceived
    = prism
        (\ (x1, x2, x3)
           -> EventMessageReceived x1 x2 x3)
        (\ x
           -> case x of
                EventMessageReceived y1 y2 y3
                  -> Right (y1, y2, y3)
                _ -> Left x)
