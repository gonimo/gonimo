{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.WebAPI where

import           Data.Proxy
import           Data.Text                (Text)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import qualified Gonimo.WebAPI.Types as Client
import           Gonimo.Types
import           Gonimo.WebAPI.Verbs
import           Servant.API

-- The development API also serves javascript
type DevelopmentAPI = GonimoAPI
  :<|> "development" :> Raw

type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
       "accounts" :> Post '[JSON] Client.AuthData
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "coffee" :> Get '[JSON] Coffee

type From    = Capture "fromClient" ClientId
type To      = Capture "toClient"   ClientId
type Channel = Capture "channelId"  Secret

type AuthGonimoAPI =
  "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation)
  -- Create an invitation

  -- with subscriber:
  -- "invitations" :> ReqBody '[JSON]' FamilyId :> SubsPost '[JSON] (InvitationId, Invitation)
  :<|> "invitations" :> ReqBody '[JSON] Secret :> Delete '[JSON] Invitation
  -- Accept an invitation

  :<|> "invitationOutbox" :> ReqBody '[JSON] Client.SendInvitation :> Post '[JSON] ()
  -- Send an invitation email/telegram message/...

  :<|> "families" :> ReqBody '[JSON] FamilyName :> Post '[JSON] FamilyId
  -- Create a family

  :<|> "socket" :> Capture "familyId" FamilyId :> From :> To :> PostCreated '[JSON] Secret
  :<|> "socket" :> Capture "familyId" FamilyId :> From :> To :> Receive '[JSON] Secret
  :<|> "socket" :> Capture "familyId" FamilyId :> From :> To :> Channel :> ReqBody '[JSON] Text :> Put '[JSON] ()
  :<|> "socket" :> Capture "familyId" FamilyId :> From :> To :> Channel :> Receive '[JSON] Text

developmentAPI :: Proxy DevelopmentAPI
developmentAPI = Proxy

gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy

authGonimoAPI :: Proxy AuthGonimoAPI
authGonimoAPI = Proxy

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
