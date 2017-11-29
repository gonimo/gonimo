{-|
Module      : Gonimo.SocketAPI.V1.View.Account
Description : Account view on data. This module defines views on data as seen by a single account independent of any family it is in.
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.SocketAPI.V1.View.Account where

import           Data.Aeson.Types (FromJSON, ToJSON (..))


import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics
import           Data.Map.Strict  (Map)

import           Gonimo.SocketAPI.Model as Model


data View
  = View { _viewCreated            :: !UTCTime
         , _viewClaimedInvitations :: !Invitations
         , _viewDevices            :: !Devices
         , _viewFamilies           :: !Families
         } deriving (Generic, Show)

instance FromJSON View
instance ToJSON View

data DeviceView
  = DeviceView { _deviceViewName         :: !Text
               , _deviceViewLastAccessed :: !UTCTime
               , _deviceViewUserAgent    :: !Text
               } deriving (Generic, Show)

instance FromJSON DeviceView
instance ToJSON DeviceView

type Devices = Map DeviceId DeviceView


data FamilyView
  = FamilyView { _familyViewName         :: !Text
               , _familyViewCreated      :: !UTCTime
               , _familyViewLastAccessed :: !UTCTime
               } deriving (Generic, Show)

instance FromJSON FamilyView
instance ToJSON FamilyView

type Families = Map FamilyId Family

data InvitationView
  = InvitationView { _invitationViewCreated    :: !UTCTime
                   , _invitationViewDelivery   :: !InvitationDelivery
                   , _invitationViewFamilyName :: !Text
                   , _invitationViewSenderName :: !(Maybe Text)
                   } deriving (Generic, Show)

instance FromJSON InvitationView
instance ToJSON InvitationView

type Invitations = Map InvitationId InvitationView
