{-|
Module      : Gonimo.SocketAPI.V1.View.Family
Description : Family view on data. This module defines views on data as seen by family members.
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.SocketAPI.V1.View.Family where

import           Data.Aeson.Types (FromJSON, ToJSON (..))
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics
import           Data.Map.Strict  (Map)

import           Gonimo.SocketAPI.Model as Model


-- | View on all relevant family data as seen by family members.
data View
  = View
    { -- | All members of the family (all accounts
      -- and their devices, including online status)
      _viewMembers     :: !Members

      -- | All currently valid inviations to this family.
    , _viewInvitations :: !Invitations
    } deriving (Generic, Show)

instance FromJSON View
instance ToJSON View


-- | We serialize all account related data into AccountView.
--
--   This includes all the devices of the account, therefore we have a simple
--   entity that can be sent in case a new family member appears. The client
--   most likely will take it apart again and manage accounts and devices in
--   their own maps.
data AccountView
  = AccountView { _accountViewCreated :: !UTCTime
                , _accountViewJoined :: !UTCTime
                , _accountViewInvitedBy :: !(Maybe InvitationDelivery)
                , _accountViewDevices :: !(Map DeviceId DeviceView)
                } deriving (Generic, Show)

instance FromJSON AccountView
instance ToJSON AccountView


data DeviceView
  = DeviceView { _deviceViewName :: !Text
               , _deviceViewLastAccessed :: !UTCTime
               , _deviceViewUserAgent :: !Text
               , _deviceViewStatus :: !(Maybe DeviceStatus)
               } deriving (Generic, Show)

instance FromJSON DeviceView
instance ToJSON DeviceView


type Members = Map AccountId AccountView


data InvitationView
  = InvitationView { _invitationViewSecret   :: !Secret
                   , _invitationViewCreated  :: !UTCTime
                   , _invitationViewDelivery :: !InvitationDelivery
                   , _invitationViewSenderId :: !AccountId
                   , _invitationViewClaimed  :: !Bool
                   } deriving (Generic, Show)

instance FromJSON InvitationView
instance ToJSON InvitationView

type Invitations = Map InvitationId InvitationView
