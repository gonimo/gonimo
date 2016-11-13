{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Gonimo.Server.State.Types where


import           Control.Concurrent.STM    (TVar)
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (StateT (..))
import           Data.Aeson.Types          (FromJSON (..), FromJSON,
                                            ToJSON (..), ToJSON (..),
                                            defaultOptions,
                                            genericToEncoding, genericToJSON)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Servant.PureScript     (jsonParseHeader, jsonParseUrlPiece)
import           Web.HttpApiData        (FromHttpApiData (..))

import           Gonimo.Server.Db.Entities (DeviceId, FamilyId)
import           Gonimo.Server.Types      (DeviceType, Secret)

type FromId = DeviceId
type ToId   = DeviceId

-- | For online session to identify a particular session
newtype SessionId = SessionId Int deriving (Ord, Eq, Show, Generic)

instance ToJSON SessionId where
  toJSON     = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SessionId

instance FromHttpApiData SessionId where
    parseUrlPiece = jsonParseUrlPiece
    parseHeader   = jsonParseHeader

-- | To identify messages on socket/channel to avoid race conditions when reading.
newtype MessageNumber = MessageNumber Int deriving (Ord, Eq, Show, Generic)

instance ToJSON MessageNumber where
  toJSON     = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON MessageNumber

instance FromHttpApiData MessageNumber where
    parseUrlPiece = jsonParseUrlPiece
    parseHeader   = jsonParseHeader


data MessageState a = Written a | Read deriving (Eq, Show)
$(makePrisms ''MessageState)

-- | A request for opening a channel on a socket.
data ChannelRequest = ChannelRequest DeviceId Secret deriving Generic

instance FromJSON ChannelRequest
instance ToJSON ChannelRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromHttpApiData ChannelRequest where
    parseUrlPiece = jsonParseUrlPiece
    parseHeader   = jsonParseHeader

-- | Writers wait for the receiver to receive a message,
-- | the reader then signals that it has read it's message
-- | and the writer afterwards removes the message. In case the receiver does not
-- | receive the message in time, the writer also removes the message.
-- | The reader never removes a message, because then it would be possible
-- | that the writer deletes someone elses message in case of a timeout.
-- |
-- | The message could already have been received and replaced by a new one and we would delete
-- | a message sent by someone else. This would have been a really nasty bug *phooooh*
data MessageBox a = MessageBox { _messageNumber :: !MessageNumber -- We need message numbers to make sure the reader does not mark the wrong message as read!
                               , _messageState  :: !(MessageState a) -- Signal the writer that the reader successfully retrieved a message.
                               } deriving (Eq, Show)
$(makePrisms ''MessageBox)


-- | Baby station calls receiveSocket: Map of it's client id to the requester's client id and the channel secret.
type ChannelSecrets = Map ToId (MessageBox ChannelRequest)

type ChannelData a  = Map (FromId, ToId, Secret) (MessageBox a)

data FamilyOnlineState = FamilyOnlineState
                       { _channelSecrets  :: ChannelSecrets
                       , _channelData     :: ChannelData Text
                       , _sessions        :: Map DeviceId (SessionId, DeviceType)
                       , _idCounter       :: Int -- Used for SessionId's currently
                       , _channelSequence :: Int -- Used to identify messages on channels/sockets.
                       } deriving (Show, Eq)

$(makeLenses ''FamilyOnlineState)

type FamilyMap = Map FamilyId (TVar FamilyOnlineState)

type OnlineState = TVar FamilyMap

type UpdateFamilyT m a = StateT FamilyOnlineState m a
type MayUpdateFamily a = UpdateFamilyT (MaybeT Identity) a
type UpdateFamily a = UpdateFamilyT Identity a

emptyFamily :: FamilyOnlineState
emptyFamily = FamilyOnlineState {
    _channelSecrets = M.empty
  , _channelData = M.empty
  , _sessions = M.empty
  , _idCounter = 0
  , _channelSequence = 0
  }
