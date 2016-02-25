module Gonimo.WebAPI where

import Servant.API
import Data.Proxy
import Gonimo.Types
import Gonimo.Server.DbEntities


type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
  "accounts" :> ReqBody '[JSON] (Maybe Credentials) :> Post '[JSON] (AccountId, AuthToken)
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "coffee" :> Get '[JSON] Coffee


type AuthGonimoAPI =
  "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation) -- Create an invitation
  :<|> "invitations" :> ReqBody '[JSON] InvitationSecret :> Delete '[JSON] Invitation -- Accept an invitation
  :<|> "families" :> ReqBody '[JSON] FamilyName -> Post '[JSON] FamilyId -- Create a family
  :<|> "invitationOutbox" :> ReqBody '[JSON] SendInvitation :> Post '[JSON] () -- Send an invitation email/telegram message/...


gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy

authGonimoAPI :: Proxy AuthGonimoAPI
authGonimoAPI = Proxy

gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
