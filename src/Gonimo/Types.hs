module Gonimo.Types where

import Data.Aeson.Types ((.:), FromJSON(..), ToJSON(..), object, Value(..), (.=), Object, pairs)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes

import Servant.API

type SenderName = Text
type InvitationSecret = Text

instance FromJSON Invitation
instance ToJSON Invitation

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


-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret ByteString
               deriving (Show)

instance FromJSON AuthToken where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    contents <- encodeUtf8 <$> o .: "contents"
    base64Decoded <- case Base64.decode contents of
      Right c -> return c
      Left err -> fail err
    if tag ==  "GonimoSecret"
      then return $ GonimoSecret base64Decoded
      else fail $ "No such constructor in AuthToken: " ++ tag
      
instance ToJSON AuthToken where
  toJSON (GonimoSecret s) = object [ "tag" .= ("GonimoSecret" :: Text)
                                   , "contents" .= decodeUtf8 (Base64.encode s)
                                   ]
  toEncoding (GonimoSecret s) = pairs ( "tag" .= ("GonimoSecret" :: Text)
                                        <> "contents" .= decodeUtf8 (Base64.encode s) )

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee

