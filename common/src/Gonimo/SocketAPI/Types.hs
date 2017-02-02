{-# LANGUAGE FlexibleInstances #-}
-- Types which are only relevant to the device.
-- They are either accepted from the device or sent to the device but have no further use
-- on the server side.

module Gonimo.SocketAPI.Types where

import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import           Data.Text           (Text)
import           GHC.Generics
import           Data.Time             (UTCTime)
import           Data.Maybe             (fromMaybe)

import           Gonimo.Types
import           Gonimo.Db.Entities


data AuthData = AuthData {
    accountId :: !AccountId
  , deviceId  :: !DeviceId
  , authToken :: !AuthToken
  } deriving (Generic, Show)

instance FromJSON AuthData
instance ToJSON AuthData where
  toEncoding = genericToEncoding defaultOptions


data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving (Eq, Ord, Generic, Show)
instance FromJSON SendInvitation
instance ToJSON SendInvitation where
  toEncoding = genericToEncoding defaultOptions


data InvitationInfo = InvitationInfo {
    invitationInfoFamily        :: !FamilyName
  , invitationInfoSendingDevice :: !Text
  , invitationInfoSendingUser   :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON InvitationInfo
instance ToJSON InvitationInfo where
  toEncoding = genericToEncoding defaultOptions

data InvitationReply = InvitationAccept | InvitationReject deriving (Generic, Show, Eq, Ord)

instance FromJSON InvitationReply
instance ToJSON InvitationReply where
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
