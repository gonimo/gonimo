{-# LANGUAGE FlexibleInstances #-}
-- Types which are only relevant to the device.
-- They are either accepted from the device or sent to the device but have no further use
-- on the server side.

module Gonimo.WebAPI.Types where

import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import           Data.Text           (Text)
import           GHC.Generics
import           Data.Time             (UTCTime)

import           Gonimo.Server.Types
import           Gonimo.Server.DbEntities


data AuthData = AuthData {
    accountId :: !AccountId
  , deviceId  :: !DeviceId
  , authToken :: !AuthToken
  } deriving (Generic, Show)

instance ToJSON AuthData

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic
instance FromJSON SendInvitation
instance ToJSON SendInvitation where
  toEncoding = genericToEncoding defaultOptions


data InvitationInfo = InvitationInfo {
    invitationInfoFamily          :: !Text
  , invitationInfoSendingDevice :: !Text
  , invitationInfoSendingUser   :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON InvitationInfo
instance ToJSON InvitationInfo where
  toEncoding = genericToEncoding defaultOptions

data InvitationReply = InvitationAccept | InvitationReject deriving (Generic, Show, Eq)

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
  { deviceInfoName = deviceName device
  , deviceInfoAccountId = deviceAccountId device
  , deviceInfoLastAccessed = deviceLastAccessed device
  , deviceInfoUserAgent = deviceUserAgent device
  }

instance ToJSON DeviceInfo where
  toEncoding = genericToEncoding defaultOptions
