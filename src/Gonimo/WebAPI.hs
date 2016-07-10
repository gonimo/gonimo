{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.WebAPI where

import           Data.Proxy
import           Data.Text (Text)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import qualified Gonimo.WebAPI.Types as Client
import           Gonimo.WebAPI.Verbs
import           Servant.API
import           Servant.API.BrowserHeader
import           Servant.Subscriber.Subscribable


type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
       "accounts" :> BrowserHeader "User-Agent" Text :> Post '[JSON] Client.AuthData
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "coffee" :> Get '[JSON] Coffee

type From    = Capture "fromClient" ClientId
type To      = Capture "toClient"   ClientId
type Channel = Capture "channelId"  Secret

type AuthGonimoAPI =
  "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation)
  -- Create an invitation

  -- Check the invitation ...
  :<|> "invitations" :> ReqBody '[JSON] Secret :> Get '[JSON] Invitation
  -- Accept the invitation ...
  :<|> "invitations" :> ReqBody '[JSON] Secret :> Delete '[JSON] ()
  -- Accept an invitation

  :<|> "invitationOutbox" :> ReqBody '[JSON] Client.SendInvitation :> Post '[JSON] ()
  -- Send an invitation email/telegram message/...

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
