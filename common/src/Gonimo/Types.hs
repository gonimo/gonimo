{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Types where




import           Control.Error.Safe     (rightZ)

import           Data.Aeson.Types       (FromJSON (..), FromJSON, ToJSON (..),
                                         ToJSON (..), Value (String),
                                         defaultOptions, genericToJSON, genericToEncoding)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Monoid
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Persist.TH

import           GHC.Generics           (Generic)
import           Control.Monad          (MonadPlus, mzero)
import           Data.Vector                    (Vector)
import           Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import           Database.Persist.Types (PersistValue(PersistText), SqlType(SqlString))
import           Database.Persist.Sql   (PersistFieldSql, sqlType)
import qualified Data.Text      as T
import           Data.Text      (Text)

data DeviceType = NoBaby
                | Baby Text
                deriving (Show, Eq, Generic)

toBabyName :: MonadPlus m => DeviceType -> m Text
toBabyName NoBaby = mzero
toBabyName (Baby name) = pure name

instance FromJSON DeviceType
instance ToJSON DeviceType

type SenderName = Text

newtype Secret = Secret ByteString deriving (Generic, Show, Read, Ord, Eq)

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _ = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
  toEncoding (Secret bs) = toEncoding $ (decodeUtf8 . Base64.encode) bs


derivePersistField "Secret"

-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               | PlaceHolder____
               deriving (Read, Show, Generic, Eq, Ord)

derivePersistField "AuthToken"

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions



data FamilyName
  = FamilyName { familyMemberName :: !Text
               , familyName :: !Text
               } deriving (Show, Generic, Eq)

parseFamilyName :: Text -> FamilyName
parseFamilyName t =
  let
    parseList :: [Text] -> Maybe FamilyName
    parseList [ mN, fN ] = Just $ FamilyName mN fN
    parseList _ = Nothing

    parseLine = parseList . map T.strip . T.splitOn ","
  in
    case parseLine t of
      Nothing -> FamilyName t t
      Just f  -> f

writeFamilyName :: FamilyName -> Text
writeFamilyName (FamilyName mN fN) =
  if mN == fN
  then mN
  else mN <> ", " <> fN

instance FromJSON FamilyName
instance ToJSON FamilyName where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance PersistField FamilyName where
  toPersistValue = PersistText . writeFamilyName
  fromPersistValue (PersistText t) = Right (parseFamilyName t)
  fromPersistValue _ = Left "A FamilyName must be PersistText"

instance PersistFieldSql FamilyName where
  sqlType _ = SqlString

type FamilyNames = Vector FamilyName

type Predicates  = Vector Text

type EmailAddress = Text


--------------------------------------------------
data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Generic)

instance FromJSON InvitationDelivery

instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

derivePersistField "InvitationDelivery"
