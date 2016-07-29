{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.WebAPI where

import           Data.Proxy
import           Data.Text (Text)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import qualified Gonimo.WebAPI.Types as Client
import Gonimo.WebAPI.Types (InvitationReply, InvitationInfo)
import           Gonimo.WebAPI.Verbs
import           Servant.API
import           Servant.API.BrowserHeader
import           Servant.Subscriber.Subscribable


type GonimoAPI =
       "accounts" :> BrowserHeader "User-Agent" Text :> Post '[JSON] Client.AuthData
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "funnyName" :> Post '[JSON] Text
  :<|> "coffee" :> Get '[JSON] Coffee

type From    = Capture "fromClient" ClientId
type To      = Capture "toClient"   ClientId
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

  :<|> "families" :> ReqBody '[JSON] FamilyName :> Post '[JSON] FamilyId
  -- Create a family

  :<|> "socket" :> SocketAPI
    
type SocketAPI =  CreateChannelR
             :<|> ReceiveSocketR
             :<|> PutChannelR
             :<|> ReceiveChannelR

type CreateChannelR   = Capture "familyId" FamilyId :> To :> ReqBody '[JSON] ClientId :> PostCreated '[JSON] Secret
type ReceiveSocketR  = Capture "familyId" FamilyId :> To :> Subscribable :> Receive '[JSON] (ClientId, Secret)
type PutChannelR     = Capture "familyId" FamilyId :> From :> To :> Channel :> ReqBody '[JSON] Text :> Put '[JSON] ()
type ReceiveChannelR = Capture "familyId" FamilyId :> From :> To :> Channel :> Subscribable :> Receive '[JSON] Text

gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy

authGonimoAPI :: Proxy AuthGonimoAPI
authGonimoAPI = Proxy

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
