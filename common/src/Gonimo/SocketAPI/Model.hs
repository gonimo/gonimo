{-|
Module      : Gonimo.SocketAPI.Model
Description : The Gonimo Data model.
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.SocketAPI.Model where

import           Data.Aeson.Types       (FromJSON (..), FromJSON, ToJSON (..),
                                         ToJSON (..), Value (String), ToJSONKey, FromJSONKey,
                                         defaultOptions, genericToJSON, genericToEncoding)
import           Data.Int         (Int64)

import           Data.Text        (Text)
import qualified Data.Text      as T
import           Data.Time        (UTCTime)
import           GHC.Generics
import           Control.Error.Safe     (rightZ)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Monoid
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)


type DbKey = Int64

-- Account:
newtype Account = Account {accountCreated :: UTCTime} deriving (Generic)
newtype AccountId = AccountId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

-- User:
data User = User { userLogin     :: !Text
                 , userPassword  :: !Text
                 , userAccountId :: !AccountId
                 } deriving (Generic)

instance FromJSON User
instance ToJSON User

newtype UserId = UserId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

-- Family:
data Family = Family { familyName              :: !FamilyName
                     , familyCreated           :: !UTCTime
                     , familyLastAccessed      :: !UTCTime
                     , familyLastUsedBabyNames :: ![Text]
                     } deriving (Generic, Show)

instance FromJSON Family
instance ToJSON Family

newtype FamilyId = FamilyId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)


-- Invitation:

data Invitation
  = Invitation { invitationSecret     :: !Secret
               , invitationFamilyId   :: !FamilyId
               , invitationCreated    :: !UTCTime
               , invitationDelivery   :: !InvitationDelivery
               , invitationSenderId   :: !DeviceId
               , invitationReceiverId :: !(Maybe AccountId)
               }
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Invitation
instance ToJSON Invitation

newtype InvitationId = InvitationId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)


-- Family Account:

data FamilyAccount
  = FamilyAccount { familyAccountAccountId :: !AccountId
                  , familyAccountFamilyId  :: !FamilyId
                  , familyAccountJoined    :: !UTCTime
                  , familyAccountInvitedBy :: !(Maybe InvitationDelivery)
                  } deriving (Generic)

instance FromJSON FamilyAccount
instance ToJSON FamilyAccount

newtype FamilyAccountId = FamilyAccountId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)


-- Device:

data Device
  = Device { deviceName         :: !(Maybe Text)
           , deviceAuthToken    :: !AuthToken
           , deviceAccountId    :: !AccountId
           , deviceLastAccessed :: !UTCTime
           , deviceUserAgent    :: !Text
           }
  deriving (Generic)

instance FromJSON Device
instance ToJSON Device

newtype DeviceId = DeviceId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

newtype Secret = Secret ByteString
  deriving (Generic, Show, Read, Ord, Eq)

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _ = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
  toEncoding (Secret bs) = toEncoding $ (decodeUtf8 . Base64.encode) bs

-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               | PlaceHolder____
               deriving (Read, Show, Generic, Eq, Ord)

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data FamilyName
  = FamilyName { familyMemberName :: !Text
               , familyNameName :: !Text
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

type EmailAddress = Text

data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON InvitationDelivery

instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- | Online status of a device.
--
--   Used in "Gonimo.SocketAPI.View.Family" and in the Server implementation.
data DeviceStatus =
  -- | Device is online and a baby station
  BabyStation
  -- | Device is just online.
  | Online
  -- | Device is offline, and therefore not accessible in any way.
  | Offline
  deriving (Generic, Show)

instance FromJSON DeviceStatus
instance ToJSON DeviceStatus
