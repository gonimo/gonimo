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
       "accounts" :> BrowserHeader "User-Agent" Text :> Post '[JSON] Client.AuthData
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "funnyName" :> Post '[JSON] Text
  :<|> "coffee" :> Get '[JSON] Coffee

type From    = Capture "fromDevice" DeviceId
type To      = Capture "toDevice"   DeviceId
type Channel = Capture "channelId"  Secret

type AuthGonimoAPI =
  -- Create an invitation
  "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation)
  -- Accept/Reject the invitation ...
  :<|> "invitations" :> Capture "invitationSecret" Secret :> ReqBody '[JSON] InvitationReply :> Delete '[JSON] ()
  -- Send an invitation email/telegram message/...
  :<|> "invitationOutbox" :> ReqBody '[JSON] Client.SendInvitation :> Post '[JSON] ()
  -- Retrieve InvitationInfo. Additional effect: The invitation is now restricted to the requesting account.
  :<|> "invitationInfo" :> Capture "invitationSecret" Secret :> Put '[JSON] InvitationInfo
  :<|> "deviceInfos" :> Capture "familyId" FamilyId :> Subscribable :> Get '[JSON] [(DeviceId, Client.DeviceInfo)]
  -- Create a family
  :<|> "families" :> FamilyAPI
  :<|> "socket" :> SocketAPI
  :<|> "onlineStatus" :> StatusAPI

type FamilyAPI = CreateFamilyR
            :<|> ListFamiliesR

type CreateFamilyR = ReqBody '[JSON] FamilyName :> Post '[JSON] FamilyId
type ListFamiliesR = Capture "accountId" AccountId :> Subscribable :> Get '[JSON] [(FamilyId, Family)]

type SocketAPI =  CreateChannelR
             :<|> ReceiveSocketR
             :<|> PutChannelR
             :<|> ReceiveChannelR

type CreateChannelR  = Capture "familyId" FamilyId :> To :> ReqBody '[JSON] DeviceId :> PostCreated '[JSON] Secret
type ReceiveSocketR  = Capture "familyId" FamilyId :> To :> Subscribable :> Receive '[JSON] (DeviceId, Secret)
-- Receive should only work if to is a baby station
type PutChannelR     = Capture "familyId" FamilyId :> From :> To :> Channel :> ReqBody '[JSON] Text :> Put '[JSON] ()
type ReceiveChannelR = Capture "familyId" FamilyId :> From :> To :> Channel :> Subscribable :> Receive '[JSON] Text

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

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
