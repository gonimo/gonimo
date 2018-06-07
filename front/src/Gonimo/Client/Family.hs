{-|
Module      : Gonimo.Client.Family
Description : User facing Family API
Copyright   : (c) Robert Klotzner, 2018

Get online devices, get accounts belonging to a family. Invite new devices,
rename the family, rename devices in this family, ...
-}
module Gonimo.Client.Family where


import           Data.Time.Clock        (UTCTime)

import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex   (DynamicMap, MDynamic)
import           Gonimo.SocketAPI.Types (codeValidTimeout)
import qualified Gonimo.SocketAPI.Types as API



-- | Configuration for creating a `Family`.
--
data Config t
  = Config { -- | Set the family's name.
             --
             --   If `identifier` is Nothing, this event will have no effect. In
             --   case this is undesired behaviour at some point, we will change
             --   `_onSetName` to take the `API.FamilyId` too.
             _onSetName          :: Event t (API.FamilyId, Text)

             -- | A new invitation will be created.
             --
             --   And will become the next `activeInvitation` if the given
             --   `API.FamilyId` matches `_identifier`.
           , _onCreateInvitation :: Event t API.FamilyId

             -- | Create a short human readable code for the given invitation.
             --
             --   Most likely you want to use `requestInvitationCode` for
             --   requesting a code for the current invitation, no matter
             --   whether it is ready yet or not.
             --
             --   We don't simply sample `_activeInvitationCode` ourselves,
             --   because this becomes nasty in those cases you want to send
             --   once it gets ready (as accomplished by
             --   `requestInvitationCode`). If we did the sampling - the code
             --   would not be ready on the `updated` event.
           , _onCreateCode       :: Event t API.InvitationId

             -- | Change the name of a device in this family.
           , _onSetDeviceName    :: Event t (API.DeviceId, Text)
           } deriving (Generic)

-- | Family data.
--
--   All data belonging to the current active family should go here. Like
--   open invitations, online devices, ...
data Family t
  = Family { -- | The `API.FamilyId` of this very family.
            _identifier           :: MDynamic t API.FamilyId

             -- | Invitations currently open for this family.
             --
             --   That is all invitations for this family that still exist: They
             --   were neither rejected nor accepted.
             --
             --   This value is `Nothing` until all invitations have been loaded from the server.
           , _openInvitations      :: MDynamic t (Invitations t)

             -- | The last invitation created by this device that was not yet accepted/rejected.
             --
             --   This value is `Nothing` if either there is no last invitation
             --   created by this device or if invitations have not yet been
             --   loaded - `openInvitations` is `Nothing` too.
           , _activeInvitation     :: MDynamic t API.InvitationId

             -- | A short (and short lived) code that can be typed in by the
             --   user in order to retrieve an invitation.
             --
             --   This code always belongs to `activeInvitation`. It is
             --   `Nothing` if none was created yet. It stays valid for `codeTimeout` seconds.
           , _activeInvitationCode :: MDynamic t API.InvitationCode

             -- | For the currently active invitation code, how long does it stay valid?
           , _codeValidUntil       :: MDynamic t UTCTime
           }


-- | Add the current `identifier` to a given event.
--
--   if `_identifier` is currently `Nothing`, the event will trigger once it
--   becomes a `Just`.
withFamily :: forall t m model val. (Reflex t, MonadHold t m, HasFamily model)
                      => model t -> Event t val -> m (Event t (API.FamilyId, val))
withFamily model = withKey (model ^. identifier)

withFamily_ :: forall t m model val. (Reflex t, MonadHold t m, HasFamily model)
                      => model t -> Event t val -> m (Event t API.FamilyId)
withFamily_ model = withKey_ (model ^. identifier)


withFamilyMay_ :: forall t model. (Reflex t, HasFamily model)
                      => model t -> Event t () -> Event t API.FamilyId
withFamilyMay_ model = withKeyMay_ (current $ model ^. identifier)

-- We Assume server/client roundtrip does not take longer than five seconds.
flightTime :: Int
flightTime = 5

-- | Delay afther a new `_activeInvitationCode` got created until it will be invalid again.
--
--   This is an approximate value, useful for displaying timeout animations to the user.
--   The value is in seconds.
codeTimeout :: Int
codeTimeout = codeValidTimeout - flightTime

-- | Map type for open invitations.
type Invitations t = DynamicMap t API.InvitationId API.Invitation


-- | Provide an event suitable for `_onCreateCode`.
--
--   The event will be tagged with `_activeInvitation`. If `_activeInvitation`
--   currently is `Nothing` then the event will be postponed until it becomes a
--   `Just`.
--
--   This means you can use this function, even if `_activeInvitation` is not
--   loaded yet, if you know it will be loaded eventually.
withInvitation :: forall t m model. (Reflex t, MonadHold t m, HasFamily model)
                      => model t -> Event t () -> m (Event t API.InvitationId)
withInvitation model = withKey_ (model ^. activeInvitation)

withInvitationMay_ :: forall t model. (Reflex t, HasFamily model)
                      => model t -> Event t () -> Event t API.InvitationId
withInvitationMay_ model = withKeyMay_ (current $ model ^. activeInvitation)

-- | Only forward an event if `_activeInvitation` is `Nothing`.
--
--   If `_openInvitations` still contains `Nothing` (invitations have not yet been
--   loaded), then the event gets delayed.
ifNoActiveInvitation :: forall t model m
                        . (Reflex t, MonadHold t m, HasFamily model)
                     => model t -> Event t () -> m (Event t API.FamilyId)
ifNoActiveInvitation model = withFamily_ model <=< waitAndFilter noInvitation
  where
    noInvitation = do
      cInvitations <- model ^. openInvitations
      cInvitation  <- model ^. activeInvitation
      pure $ if isNothing cInvitations
             then Nothing
             else Just $ isNothing cInvitation

-- | Only forward an event if `_activeInvitationCode` is `Nothing`.
--
--   If `_activeInvitation` still contains `Nothing`, then the event gets
--   delayed until `_activeInvitation` hold an invitation.
ifNoActiveInvitationCode :: forall t model
  . (Reflex t, HasFamily model)
  => model t -> MDynamic t Bool
ifNoActiveInvitationCode model = do
      cInvitation <- model ^. activeInvitation
      cCode  <- model ^. activeInvitationCode
      pure $ if isNothing cInvitation
             then Nothing
             else Just $ isNothing cCode

instance Reflex t => Default (Config t) where
  def = mempty

instance Reflex t => Semigroup (Config t) where
  a <> b = Config { _onSetName          = leftmost [_onSetName a, _onSetName b]
                  , _onCreateInvitation = leftmost [ _onCreateInvitation a
                                                   , _onCreateInvitation b
                                                   ]
                  , _onCreateCode       = leftmost [_onCreateCode a, _onCreateCode b]
                  , _onSetDeviceName    = leftmost [ _onSetDeviceName a
                                                   , _onSetDeviceName b
                                                   ]
                  }

instance Reflex t => Monoid (Config t) where
  mempty = Config never never never never
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onSetName <$> ev)
      <*> doSwitch never (_onCreateInvitation <$> ev)
      <*> doSwitch never (_onCreateCode <$> ev)
      <*> doSwitch never (_onSetDeviceName <$> ev)

instance Flattenable Family where
  flattenWith doSwitch ev
    = Family
      <$> flattenDynamic doSwitch (_identifier <$> ev)
      <*> flattenDynamic doSwitch (_openInvitations <$> ev)
      <*> flattenDynamic doSwitch (_activeInvitation <$> ev)
      <*> flattenDynamic doSwitch (_activeInvitationCode <$> ev)
      <*> flattenDynamic doSwitch (_codeValidUntil <$> ev)

instance Reflex t => Default (Family t) where
  def = Family (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing) (pure Nothing)

-- Auto generated lenses:

class HasConfig a42 where
  config :: Lens' (a42 t) (Config t)

  onSetName :: Lens' (a42 t) (Event t (API.FamilyId, Text))
  onSetName = config . go
    where
      go :: Lens' (Config t) (Event t (API.FamilyId, Text))
      go f config' = (\onSetName' -> config' { _onSetName = onSetName' }) <$> f (_onSetName config')


  onCreateInvitation :: Lens' (a42 t) (Event t API.FamilyId)
  onCreateInvitation = config . go
    where
      go :: Lens' (Config t) (Event t API.FamilyId)
      go f config' = (\onCreateInvitation' -> config' { _onCreateInvitation = onCreateInvitation' }) <$> f (_onCreateInvitation config')


  onCreateCode :: Lens' (a42 t) (Event t API.InvitationId)
  onCreateCode = config . go
    where
      go :: Lens' (Config t) (Event t API.InvitationId)
      go f config' = (\onCreateCode' -> config' { _onCreateCode = onCreateCode' }) <$> f (_onCreateCode config')


  onSetDeviceName :: Lens' (a42 t) (Event t (API.DeviceId, Text))
  onSetDeviceName = config . go
    where
      go :: Lens' (Config t) (Event t (API.DeviceId, Text))
      go f config' = (\onSetDeviceName' -> config' { _onSetDeviceName = onSetDeviceName' }) <$> f (_onSetDeviceName config')


instance HasConfig Config where
  config = id

class HasFamily a42 where
  family :: Lens' (a42 t) (Family t)

  identifier :: Lens' (a42 t) (MDynamic t API.FamilyId)
  identifier = family . go
    where
      go :: Lens' (Family t) (MDynamic t API.FamilyId)
      go f family' = (\identifier' -> family' { _identifier = identifier' }) <$> f (_identifier family')


  openInvitations :: Lens' (a42 t) (MDynamic t (Invitations t))
  openInvitations = family . go
    where
      go :: Lens' (Family t) (MDynamic t (Invitations t))
      go f family' = (\openInvitations' -> family' { _openInvitations = openInvitations' }) <$> f (_openInvitations family')


  activeInvitation :: Lens' (a42 t) (MDynamic t API.InvitationId)
  activeInvitation = family . go
    where
      go :: Lens' (Family t) (MDynamic t API.InvitationId)
      go f family' = (\activeInvitation' -> family' { _activeInvitation = activeInvitation' }) <$> f (_activeInvitation family')


  activeInvitationCode :: Lens' (a42 t) (MDynamic t API.InvitationCode)
  activeInvitationCode = family . go
    where
      go :: Lens' (Family t) (MDynamic t API.InvitationCode)
      go f family' = (\activeInvitationCode' -> family' { _activeInvitationCode = activeInvitationCode' }) <$> f (_activeInvitationCode family')


  codeValidUntil :: Lens' (a42 t) (MDynamic t UTCTime)
  codeValidUntil = family . go
    where
      go :: Lens' (Family t) (MDynamic t UTCTime)
      go f family' = (\codeValidUntil' -> family' { _codeValidUntil = codeValidUntil' }) <$> f (_codeValidUntil family')


instance HasFamily Family where
  family = id

