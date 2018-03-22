{-# LANGUAGE FlexibleInstances #-}
-- Types which are only relevant to the device.
-- They are either accepted from the device or sent to the device but have no further use
-- on the server side.

module Gonimo.SocketAPI.Types where

import           Data.Aeson.Types (FromJSON, ToJSON (..), defaultOptions,
                                   genericToEncoding)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics

import           Gonimo.Types
import           Gonimo.SocketAPI.Internal
import           Gonimo.SocketAPI.Invitation.Internal


data AuthData = AuthData {
    accountId :: !AccountId
  , deviceId  :: !DeviceId
  , authToken :: !AuthToken
  } deriving (Generic, Show)

instance FromJSON AuthData
instance ToJSON AuthData where
  toEncoding = genericToEncoding defaultOptions


data DeviceInfo = DeviceInfo
  { deviceInfoName :: !Text
  , deviceInfoAccountId :: !AccountId
  , deviceInfoLastAccessed :: !UTCTime
  , deviceInfoUserAgent :: !Text
  } deriving (Generic, Show)


fromDevice :: Device -> DeviceInfo
fromDevice device = DeviceInfo
  { deviceInfoName = fromMaybe "" $ deviceName device
  , deviceInfoAccountId = deviceAccountId device
  , deviceInfoLastAccessed = deviceLastAccessed device
  , deviceInfoUserAgent = deviceUserAgent device
  }

instance ToJSON DeviceInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DeviceInfo

type FromId = DeviceId
type ToId = DeviceId

data SessionDescription
  = SessionDescription { _sdp :: Text
                       , _type_ :: RTCSdpType
                       } deriving (Generic, Eq, Ord, Show)

-- Identical to the one in GHCJS.DOM.JSFFI.Generated.Enums, duplicated for avoiding dependency on ghcjs-dom in gonimo-common. (Translation function is necessary.)
data RTCSdpType = RTCSdpTypeOffer
                | RTCSdpTypePranswer
                | RTCSdpTypeAnswer
                | RTCSdpTypeRollback
                deriving (Show, Read, Eq, Ord, Generic)

data IceCandidate
  = IceCandidate { _sdpMLineIndex :: Maybe Int
                 , _sdpMid :: Maybe Text
                 , _candidate :: Text
                 } deriving (Generic, Eq, Ord, Show)

data Message
  = MsgSessionDescriptionOffer SessionDescription
  | MsgSessionDescriptionAnswer SessionDescription
  | MsgIceCandidate IceCandidate
  | MsgCloseConnection deriving (Generic, Eq, Ord, Show)

instance FromJSON SessionDescription
instance ToJSON SessionDescription where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RTCSdpType
instance ToJSON RTCSdpType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON IceCandidate
instance ToJSON IceCandidate where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Message
instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

-- For now, just copied Db types:

-- Account:
newtype Account = Account {accountCreated :: UTCTime} deriving (Generic)
newtype AccountId = AccountId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)

-- User:
data User = User { userLogin     :: !Text
                 , userPassword  :: !Text
                 , userAccountId :: !AccountId
                 } deriving (Generic)

instance FromJSON User
instance ToJSON User

newtype UserId = UserId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)

-- Family:
data Family = Family { familyName              :: !FamilyName
                     , familyCreated           :: !UTCTime
                     , familyLastAccessed      :: !UTCTime
                     , familyLastUsedBabyNames :: ![Text]
                     } deriving (Generic, Show)

instance FromJSON Family
instance ToJSON Family

newtype FamilyId = FamilyId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)


-- Invitation:



-- Family Account:

data FamilyAccount
  = FamilyAccount { familyAccountAccountId :: !AccountId
                  , familyAccountFamilyId  :: !FamilyId
                  , familyAccountJoined    :: !UTCTime
                  , familyAccountInvitedBy :: !(Maybe InvitationDelivery)
                  } deriving (Generic)

instance FromJSON FamilyAccount
instance ToJSON FamilyAccount

newtype FamilyAccountId = FamilyAccountId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)


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

newtype DeviceId = DeviceId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)
