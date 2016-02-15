module Gonimo.WebAPI where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Servant.API
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Proxy
import Data.Time.Calendar (Day)

type EmailAddress = Text
type FamilyId = Int
type SenderName = Text
type InvitationId = Int
type InvitationSecret = Text
type SenderId = Int
type InboxId = Int

data Invitation = Invitation {
    invitationSecret :: InvitationSecret
  , familyId :: FamilyId
  , created :: Day
  , transmittedBy :: InvitationDelivery
  } deriving (Show, Generic)
                  
data InvitationDelivery = EmailInvitation EmailAddress
                      | OtherInvitation
                      deriving (Show, Generic)
                        

instance FromJSON InvitationDelivery
instance ToJSON InvitationDelivery

instance FromJSON Invitation
instance ToJSON Invitation


data Credentials = Credentials {
    userEmail :: EmailAddress
  , userPassword :: Text
  } deriving Generic

instance FromJSON Credentials
instance ToJSON Credentials

data AccountData = AccountData {
    credentials :: Maybe Credentials
  , secret :: AuthToken
  }

type AccountId  = Int

-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Text
               | SendersSecret Text deriving (Show, Generic)
instance FromJSON AuthToken
instance ToJSON AuthToken

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee


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
  :<|> "families" :> Capture "familyId" FamilyId :> "senders" :> Get '[JSON] [(SenderName, [SenderId])]
  :<|> "families" :> Capture "familyId" FamilyId :> "senders" :> ReqBody '[JSON] SenderName :> Post '[JSON] SenderId
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Post '[JSON] InboxId
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Get '[JSON] [InboxId]
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "sender" :> ReqBody '[PlainText] Text :> Put '[PlainText] ()
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "sender" :> Get '[PlainText] Text
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "sender" :> Delete '[PlainText] ()
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "receiver" :> ReqBody '[PlainText] Text :> Put '[PlainText] ()
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "receiver" :> Get '[PlainText] Text
  :<|> "families" :> Caputure "familyId" FamilyId :> "senders" :> Capture "senderName" SenderName :> Capture "senderId" SenderId :> "inboxes" :> Capture "inbox" InboxId :> "receiver" :> Delete '[PlainText] ()



gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy
       
gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
