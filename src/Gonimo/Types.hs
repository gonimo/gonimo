module Gonimo.Types where



-- import Data.Aeson.Types ((.:), FromJSON(..), ToJSON(..), object, Value(..), (.=), pairs, FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)
import Data.Aeson.Types ((.:), FromJSON(..), ToJSON(..), object, Value(..), (.=), FromJSON, ToJSON(..), defaultOptions, genericToJSON)




import Data.Text (Text)


import GHC.Generics (Generic)
import Gonimo.Server.DbTypes
import Gonimo.Server.DbEntities
import Servant.Common.Text (FromText (..))

import qualified Data.Text as T



type SenderName = Text

data UserName = UserNameEmail EmailAddress
              | UserNamePhone Text
              deriving Generic

getUserEmail :: UserName -> Maybe EmailAddress
getUserEmail (UserNameEmail addr) = Just addr
getUserEmail _ = Nothing

getUserPhone :: UserName -> Maybe Text
getUserPhone (UserNamePhone number) = Just number
getUserPhone _ = Nothing


instance FromJSON UserName
instance ToJSON UserName where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions


data Credentials = Credentials {
    userName :: UserName
  , userPassword :: Text
  } deriving Generic

instance FromJSON Credentials
instance ToJSON Credentials where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions


data AccountData = AccountData {
    credentials :: Maybe Credentials
  , secret :: AuthToken
  }


-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               deriving (Show, Generic)

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

instance FromText AuthToken where
  fromText t = case T.words t of
    ["GonimoSecret", contents] -> GonimoSecret <$> fromText contents
    _ -> Nothing

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic

instance FromJSON SendInvitation

type FamilyName = Text
