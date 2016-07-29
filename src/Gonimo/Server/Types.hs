{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Server.Types where




import           Control.Error.Safe     (rightZ)

import           Data.Aeson.Types       (FromJSON (..), FromJSON, ToJSON (..),
                                         ToJSON (..), Value (String),
                                         defaultOptions, genericToJSON)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Monoid
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Persist.TH
import           Debug.Trace            (trace)

import           GHC.Generics           (Generic)
import           Servant.PureScript     (jsonParseHeader, jsonParseUrlPiece,
                                         jsonToHeader, jsonToUrlPiece)
import           Web.HttpApiData        (FromHttpApiData (..),
                                         ToHttpApiData (..))

type SenderName = Text


newtype Secret = Secret ByteString deriving (Generic, Show, Read, Ord, Eq)

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _ = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
  toEncoding (Secret bs) = toEncoding $ (decodeUtf8 . Base64.encode) bs

instance FromHttpApiData Secret where
  parseUrlPiece = jsonParseUrlPiece . logInput
    where logInput input = trace (T.unpack $ "FromHttpApiData Secret, got:" <> input) input
  parseHeader   = jsonParseHeader

instance ToHttpApiData Secret where
  toUrlPiece = jsonToUrlPiece
  toHeader   = jsonToHeader

derivePersistField "Secret"

-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               deriving (Read, Show, Generic)

derivePersistField "AuthToken"

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

instance FromHttpApiData AuthToken where
    parseUrlPiece :: Text -> Either Text AuthToken
    parseUrlPiece = jsonParseUrlPiece
    parseHeader   = jsonParseHeader

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

data FunnyWordType = FunnyPrefix
                   | FunnyCharacter
                   | FunnySuffix
                   deriving (Read, Show, Generic)

instance FromJSON FunnyWordType

instance ToJSON FunnyWordType where
  toJSON = genericToJSON defaultOptions

derivePersistField "FunnyWordType"
