{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.WebAPI where

import           Data.Proxy
import           Data.Text                       (Text)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import           Gonimo.WebAPI.Types             (InvitationInfo,
                                                  InvitationReply)
import qualified Gonimo.WebAPI.Types             as Client
import           Gonimo.WebAPI.Verbs
import           Servant.API
import           Servant.API.BrowserHeader
import           Servant.Subscriber.Subscribable
import           Gonimo.Server.State


type GonimoAPI =
       "accounts"                       :> BrowserHeader "User-Agent" Text :> Post '[JSON] Client.AuthData
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "funnyName"                      :> Post '[JSON] Text
  :<|> "coffee"                         :> Get '[JSON] Coffee

type From    = Capture "fromDevice" DeviceId
type To      = Capture "toDevice"   DeviceId
type Channel = Capture "channelId"  Secret

type AuthGonimoAPI = "invitations"  :> InvitationsAPI
                :<|> "accounts"     :> AccountsAPI
                :<|> "families"     :> FamiliesAPI
                :<|> "socket"       :> SocketAPI
                :<|> "onlineStatus" :> StatusAPI


-- invitations API:
type InvitationsAPI = CreateInvitationR
                 :<|> AnswerInvitationR
                 :<|> SendInvitationR
                 :<|> PutInvitationInfoR

type CreateInvitationR = Capture "familyId" FamilyId :> Post '[JSON] (InvitationId, Invitation)

type AnswerInvitationR  =
  Capture "invitationSecret" Secret :> ReqBody '[JSON] InvitationReply :> Delete '[JSON] ()

-- Send an invitation email/telegram message/...
type SendInvitationR    =
  "outbox"                          :> ReqBody '[JSON] Client.SendInvitation :> Post '[JSON] ()

-- | Retrieve information about an invitation and claim it - no other device will
--   no be able to claim or accept this invitation.
type PutInvitationInfoR =
  "info"                            :> Capture "invitationSecret" Secret :> Put '[JSON] InvitationInfo


-- AccountsAPI:
type AccountsAPI = Capture "accountId" AccountId :> AccountAPI
type AccountAPI  = GetAccountFamiliesR

type GetAccountFamiliesR = "families" :> Subscribable :> Get '[JSON] [(FamilyId, Family)]


-- FamiliesAPI:
type FamiliesAPI = CreateFamilyR
              :<|> Capture "familyId" FamilyId :> FamilyAPI

type CreateFamilyR = ReqBody '[JSON] FamilyName :> Post '[JSON] FamilyId


-- FamilyAPI:
type FamilyAPI = GetLastBabyNamesR
            :<|> GetFamilyDevicesR

type GetLastBabyNamesR = Subscribable  :> Get '[JSON] [Text]
type GetFamilyDevicesR = "deviceInfos" :> Subscribable :> Get '[JSON] [(DeviceId, Client.DeviceInfo)]

-- SocketAPI:
type SocketAPI =  CreateChannelR
             :<|> ReceiveSocketR
             :<|> PutChannelR
             :<|> ReceiveChannelR

type CreateChannelR  = Capture "familyId" FamilyId :> To :> ReqBody '[JSON] DeviceId :> PostCreated '[JSON] Secret
type ReceiveSocketR  = Capture "familyId" FamilyId :> To :> Subscribable :> Receive '[JSON] (DeviceId, Secret)
-- Receive should only work if to is a baby station
type PutChannelR     = Capture "familyId" FamilyId :> From :> To :> Channel :> ReqBody '[JSON] Text :> Put '[JSON] ()
type ReceiveChannelR = Capture "familyId" FamilyId :> From :> To :> Channel :> Subscribable :> Receive '[JSON] Text


-- StatusAPI:
type StatusAPI =   RegisterR
              :<|> UpdateR
              :<|> DeleteR
              :<|> ListDevicesR

type RegisterR    = Capture "familyId" FamilyId :> ReqBody '[JSON] (DeviceId, DeviceType) :> PostCreated '[JSON] ()
type UpdateR      = Capture "familyId" FamilyId :> Capture "deviceId" DeviceId  :> ReqBody '[JSON] DeviceType :> Put '[JSON] ()
type DeleteR      = Capture "familyId" FamilyId :> Capture "deviceId" DeviceId  :> Delete '[JSON] ()
type ListDevicesR = Capture "familyId" FamilyId :> Subscribable :> Get '[JSON] [(DeviceId,DeviceType)]

gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy

authGonimoAPI :: Proxy AuthGonimoAPI
authGonimoAPI = Proxy

invitationsAPI :: Proxy InvitationsAPI
invitationsAPI = Proxy

accountsAPI :: Proxy AccountsAPI
accountsAPI = Proxy

familiesAPI :: Proxy FamiliesAPI
familiesAPI = Proxy

socketAPI :: Proxy SocketAPI
socketAPI = Proxy

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
