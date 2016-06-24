{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Types where



import           Data.Aeson.Types      (FromJSON (..), FromJSON, ToJSON (..),
                                        ToJSON (..), defaultOptions,
                                        genericToJSON)


import           GHC.Generics          (Generic)
import           Gonimo.Server.DbTypes
import           Web.HttpApiData       (FromHttpApiData (..),
                                        parseUrlPieceWithPrefix)

import           Data.Text             (Text)
import           Control.Error.Safe (rightZ)
import           Data.Aeson.Types       (FromJSON (..), ToJSON (..), Value (String), defaultOptions, genericToJSON)
import           Data.ByteString (ByteString)

import qualified Data.ByteString.Base64 as Base64
import           Data.Bifunctor
import           Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Database.Persist.TH
import           GHC.Generics (Generic)
{-import           Servant.Common.Text (FromText (..), ToText (..))-}
import           Web.HttpApiData (FromHttpApiData(..))

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



type FamilyName = Text

type EmailAddress = Text


--------------------------------------------------
data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Generic)

instance FromJSON InvitationDelivery

instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  -- toEncoding = genericToEncoding defaultOptions

derivePersistField "InvitationDelivery"
--------------------------------------------------

newtype Secret = Secret ByteString deriving (Generic, Show, Read)

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _ = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
u
instance FromHttpApiData Secret where
    parseUrlPiece = bimap T.pack Secret . Base64.decode . encodeUtf8

derivePersistField "Secret"
