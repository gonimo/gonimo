{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.WebAPI where

import Data.Text (Text)
import Servant.API
import Data.Proxy
import Gonimo.Types
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes

-- The development API also serves javascript
type DevelopmentAPI = GonimoAPI
  :<|> "development" :> Raw

type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
  --"clients" :> ReqBody '[JSON] (Maybe Credentials) :> Post '[JSON] (AccountId, AuthToken)
  Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "coffee" :> Get '[JSON] Coffee


type AuthGonimoAPI =
  "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation)
  -- Create an invitation

  -- with subscriber:
  -- "invitations" :> ReqBody '[JSON]' FamilyId :> SubsPost '[JSON] (InvitationId, Invitation)
  :<|> "invitations" :> ReqBody '[JSON] Secret :> Delete '[JSON] Invitation
  -- Accept an invitation
  --
  :<|> "invitationOutbox" :> ReqBody '[JSON] SendInvitation :> Post '[JSON] ()
  -- Send an invitation email/telegram message/...
  --
  :<|> "families" :> ReqBody '[JSON] FamilyName :> Post '[JSON] FamilyId
  -- Create a family

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> ReqBody '[JSON] BabyName :> PostCreated '[JSON] ()
  -- Create a "mail box" -- limit number of connections! & also limit number of
  -- mailboxes per family to something reasonable! - DoS! & Only allow creation
  -- of mailboxes for families with more than one member! + Some IP limit? + Log
  -- total number of mailboxes - warn when critical! -> Make sure the needed
  -- client resources for exhausting the server are substantial!

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Delete '[JSON] ()
  -- Delete a mailbox when no longer needed -> Be nice!

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> "keepAlive" :> Post '[JSON] ()
  -- Keep alive - we need to cleanup when no longer used, alternatively the keep
  -- alive handler can be triggered via our websocket pong. HOW?

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> PostCreated '[JSON] ConnectionId
  -- Connect to baby

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> Delete '[JSON] ()
  -- Delete a connection when you are done with it - this is just being nice to
  -- the server.

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> "InMessage" :> ReqBody '[JSON] Text :> Put '[JSON] ()
  -- Send message, this function should only return when the receiver got the
  -- message - Make sure probably large messages don't fill up memory.

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> "InMessage" :> Read '[JSON] Text
  -- Baby reads message

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> "OutMessage" :> ReqBody '[SOMETEXT] Text :> Put '[JSON] ()
  -- Baby can send messages

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> "OutMessage" :> Read '[TEXT] Text
  -- Client can read messages from baby

  -- :<|> "families" :> Capture "familyId" FamilyId :> "babies" :> Capture "babyName" BabyName :> Capture "connectionId" ConnectionId :> "KeepAlive" :> Post '[JSON] ()
  -- Keep this connection alive. Clients have to show that they are still there
  -- - either by sending messages or by keep alive! - Make sure using server
  -- resources also means client resources! DoS!


developmentAPI :: Proxy DevelopmentAPI
developmentAPI = Proxy

gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy

authGonimoAPI :: Proxy AuthGonimoAPI
authGonimoAPI = Proxy

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
