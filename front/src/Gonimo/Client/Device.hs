{-|
Module      : Gonimo.Client.Device
Description : User facing Device API.
Copyright   : (c) Robert Klotzner, 2018

This is basically the entry point to any server communication. The device belongs to an account which in turn is a member of families. A device can be online in a family `onSelectFamily` and a device can be a baby station: `deviceType`, `API.Baby`.
-}
module Gonimo.Client.Device where

import           Data.Map               (Map)

import           Gonimo.Client.Prelude
import           Gonimo.SocketAPI.Types (InvitationInfo, InvitationReply)
import           Gonimo.Types           (InvitationSecret)



-- | Configuration for creating a device.
--
data Config t
  = Config { -- | Claim an invitation by providing it's secret.
             --
             --   When the user clicks an invitation link it gets claimed,
             --   making the invitation unavailable for other parties.
             _onSetDeviceType :: Event t API.DeviceType

             -- | Select one of the account's families.
             --
             --   The selected family is the family we are currently operating
             --   on, it is the family that can be accessed via
             --   `selectedFamily`.
           , _onSelectFamily :: Event t API.FamilyId

             -- | Answer an invitation. (Decline/accept it.)
           , _onAnswerInvitation :: Event t [(InvitationSecret, InvitationReply)]
           } deriving (Generic)

-- | Device data.
data Device t
  = Device { -- | The currently selected family (if any).
             _selectedFamily :: Dynamic t (Maybe Family)
           , _deviceType     :: Dynamic t DeviceType
           }


instance Reflex t => Default (Config t) where
  def = mempty

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = memptydefault
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onSetDeviceType    <$> ev)
      <*> doSwitch never (_onSelectFamily     <$> ev)
      <*> doSwitch never (_onAnswerInvitation <$> ev)

-- Auto generated lenses:
