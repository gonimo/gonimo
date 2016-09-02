{-# LANGUAGE FlexibleInstances #-}
-- Types which are only relevant to the Client.
-- They are either accepted from the client or sent to the client but have no further use
-- on the server side.

module Gonimo.WebAPI.Types where

import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToJSON, genericToEncoding)
import           Data.Text           (Text)
import           GHC.Generics
import           Data.Time             (UTCTime)

import           Gonimo.Server.Types
import           Gonimo.Server.DbEntities


data AuthData = AuthData {
    accountId :: !AccountId
  , clientId  :: !ClientId
  , authToken :: !AuthToken
  } deriving (Generic, Show)

instance ToJSON AuthData

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic
instance FromJSON SendInvitation
instance ToJSON SendInvitation where
  toEncoding = genericToEncoding defaultOptions


data InvitationInfo = InvitationInfo {
    invitationInfoFamily          :: !Text
  , invitationInfoSendingClient :: !Text
  , invitationInfoSendingUser   :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON InvitationInfo
instance ToJSON InvitationInfo where
  toEncoding = genericToEncoding defaultOptions

data InvitationReply = InvitationAccept | InvitationReject deriving (Generic, Show, Eq)

instance FromJSON InvitationReply
instance ToJSON InvitationReply where
  toEncoding = genericToEncoding defaultOptions

data ClientInfo = ClientInfo
  { clientInfoName :: !Text
  , clientInfoAccountId :: !AccountId
  , clientInfoLastAccessed :: !UTCTime
  , clientInfoUserAgent :: !Text
  } deriving (Generic, Show)


fromClient :: Client -> ClientInfo
fromClient client = ClientInfo
  { clientInfoName = clientName client
  , clientInfoAccountId = clientAccountId client
  , clientInfoLastAccessed = clientLastAccessed client
  , clientInfoUserAgent = clientUserAgent client
  }

instance ToJSON ClientInfo where
  toEncoding = genericToEncoding defaultOptions
