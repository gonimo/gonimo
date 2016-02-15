module Gonimo.Types where

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

data UserName = UserNameEmail EmailAddress
           -- | UserNameTelephone Text
              deriving Generic

instance FromJSON UserName
instance ToJSON UserName
                         

data Credentials = Credentials {
    userName :: UserName
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

