module Gonimo.WebAPI where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Servant.API
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Proxy
import Data.Time.Calendar (Day)
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes
import Gonimo.Types


type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
       "accounts" :> ReqBody '[JSON] (Maybe Credentials) :> Post '[JSON] AuthToken
  -- "Login", or retrieve your secret, by passing your credentials:
  -- Conceptually this is a get, as nothing is changed on the server and only data is retrieved.
  -- However, we don't want to transmit user details in the URI nor do we want the received AuthToken to be cached.
  :<|> "accounts" :> "account" :> ReqBody '[JSON] Credentials :> Post '[JSON] AuthToken
  :<|> Header "Authorization" AuthToken :> AuthGonimoAPI
  :<|> "coffee" :> Get '[JSON] Coffee

type AuthGonimoAPI =
       "accounts" :> "account" :> "families" :> Post '[JSON] [FamilyId] 
  :<|> "invitations" :> QueryParam "familyId" FamilyId :> Get '[JSON] [(InvitationId, Invitation)]
  :<|> "invitations" :> ReqBody '[JSON] FamilyId :> Post '[JSON] (InvitationId, Invitation)
  :<|> "invitations" :> Capture "invitationId" InvitationId :> Get '[JSON] Invitation
  :<|> "invitations" :> Capture "invitationId" InvitationId :> Delete '[JSON] ()
  :<|> "invitations" :> Capture "invitationSecret" InvitationSecret :> Delete '[JSON] FamilyId 
  :<|> "invitations" :> Capture "invitationId" InvitationId :> "delivery" :> ReqBody '[JSON] InvitationDelivery :> Post '[JSON] ()
  -- Create a family:
  :<|> "families" :> Post '[JSON] FamilyId



gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy
       
gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
