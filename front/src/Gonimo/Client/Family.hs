{-|
Module      : Gonimo.Client.Family
Description : User facing Family API
Copyright   : (c) Robert Klotzner, 2018

Get online devices, get accounts belonging to a family. Invite new devices,
rename the family, rename devices in this family, ...
-}
module Gonimo.Client.Family where

import           Data.Map               (Map)

import           Gonimo.Client.Prelude
import           Gonimo.SocketAPI.Types (InvitationInfo, InvitationReply)
import           Gonimo.Types           (InvitationSecret)



-- | Configuration for creating a `Family`.
--
data Config t
  = Config { -- | Claim an invitation by providing it's secret.
             --
             --   When the user clicks an invitation link it gets claimed,
             --   making the invitation unavailable for other parties.
             _onSetName :: Event t Text

             -- | A new invitation will be created.
             --
             --   And will become the next `activeInvitation`.
           , _onCreateInvitation :: Event t ()

             -- | Create a short human readable code for the current `activeInvitation`.
             --
             --   Once created, `activeInvitationCode` will be set accordingly.
           , _onCreateCode :: Event t ()

           -- To be implemented ...
           -- , _onSetDeviceName :: Event t (DeviceId, Text)
           } deriving (Generic)

-- | Family data.
--   All data belonging to the current active family should go here. Like
--   claimed invitations or user name, ...
data Family t
  = Family { -- | Invitations currently open for this family.
             --
             --   That is all invitations for this family that still exist: They
             --   were neither rejected nor accepted.
             _openInvitations :: Dynamic t Invitations

             -- | The last invitation created by this device that was not yet accepted/rejected.
           , _activeInvitation :: Dynamic t (Maybe InvitationId)

             -- | A short (and short lived) code that can be typed in by the
             --   user in order to retrieve an invitation.
             --
             --   This code always belongs to `activeInvitation`.
           , _activeInvitationCode :: Dynamic t (Maybe API.InvitationCode)
            }

-- | Map type for open invitations.
--
type Invitations = Map InvitationId InvitationInfo

instance Reflex t => Default (Config t) where
  def = memptydefault

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = memptydefault
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onSetName <$> ev)
      <*> doSwitch never (_onCreateInvitation <$> ev)
      <*> doSwitch never (_onCreateCode <$> ev)

-- Auto generated lenses:

