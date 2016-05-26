{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Gonimo.Types where



-- import Data.Aeson.Types ((.:), FromJSON(..), ToJSON(..), object, Value(..), (.=), pairs, FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)
import           Data.Aeson.Types         (FromJSON (..), FromJSON, ToJSON (..),
                                           ToJSON (..), Value (..),
                                           defaultOptions, genericToJSON,
                                           object, (.:), (.=))




import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)

import           GHC.Generics             (Generic)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.DbTypes
{-import           Servant.Common.Text      (FromText (..))-}
import           Web.HttpApiData          (FromHttpApiData (..), parseUrlPieceWithPrefix)

import qualified Data.Text                as T



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
    userName     :: UserName
  , userPassword :: Text
  } deriving Generic

instance FromJSON Credentials
instance ToJSON Credentials where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions


data AccountData = AccountData {
    credentials :: Maybe Credentials
  , secret      :: AuthToken
  } deriving Generic


-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               deriving (Show, Generic)

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

instance FromHttpApiData AuthToken where
    parseUrlPiece :: Text -> Either Text AuthToken
    parseUrlPiece x = do gsecret :: Text <- parseUrlPieceWithPrefix "GonimoSecret " x
                         GonimoSecret <$> parseUrlPiece gsecret

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic

instance FromJSON SendInvitation

type FamilyName = Text
