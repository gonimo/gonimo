module Gonimo.Types where

import Data.Aeson.Types ((.:), FromJSON(..), ToJSON(..), object, Value(..), (.=), pairs, FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Monoid ((<>))

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import GHC.Generics (Generic)
import Gonimo.Server.DbTypes
import Servant.Common.Text (FromText (..))
import Control.Error.Safe (rightMay)
import qualified Data.Text as T



type SenderName = Text
type InvitationSecret = Text

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
  toEncoding = genericToEncoding defaultOptions
                         

data Credentials = Credentials {
    userName :: UserName
  , userPassword :: Text
  } deriving Generic

instance FromJSON Credentials
instance ToJSON Credentials where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
 

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
  parseJSON _ = fail "Expected an object when parsing an AuthToken."
      
instance ToJSON AuthToken where
  toJSON (GonimoSecret s) = object [ "tag" .= ("GonimoSecret" :: Text)
                                   , "contents" .= decodeUtf8 (Base64.encode s)
                                   ]
  toEncoding (GonimoSecret s) = pairs ( "tag" .= ("GonimoSecret" :: Text)
                                        <> "contents" .= decodeUtf8 (Base64.encode s) )

instance FromText AuthToken where
  fromText t =
    let
      mval = case T.words t of
        [tag, contents] -> Just (tag, contents)
        _ -> Nothing
      mdecoded = mval >>= rightMay . Base64.decode . encodeUtf8 . snd
      mtag = fst <$> mval
    in
      case mtag of
        Just "GonimoSecret" -> GonimoSecret <$> mdecoded
        _ -> Nothing

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

