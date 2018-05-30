{-|
Module      : Gonimo.Client.Device
Description : User facing Device API.
Copyright   : (c) Robert Klotzner, 2018

This is basically the entry point to any server communication. The device belongs to an account which in turn is a member of families. A device can be online in a family `onSelectFamily` and a device can be a baby station: `deviceType`, `API.Baby`.
-}
module Gonimo.Client.Device where


import           Gonimo.Client.Family   (Family)
import qualified Gonimo.Client.Family   as Family
import           Gonimo.Client.Prelude
import qualified Gonimo.SocketAPI.Types as API
import           Gonimo.Types           (DeviceType (..))



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

             --   If the `onSelectedFamily` contains `Nothing` this either means that no
             --   `FamilyId` was selected yet or that the `FamilyId`s of this account have
             --   not yet been loaded. You can distinguish the two cases by checking the
             --   `Account.famililies` `Dynamic`. If it is `Nothing` then families are not
             --   loaded yet. The difference is not only relevant for display purposes, but
             --   also if the families are not yet loaded: No action is required (just wait
             --   for it to be loaded). If they are loaded but this is still `Nothing` you
             --   might want to perform some action so some `Family` is selected.
           , _onSelectFamily  :: Event t API.FamilyId

             -- | The configuration for `selectedFamily`.
           , _familyConfig    :: Family.Config t

           } deriving (Generic)

-- | Device data.
data Device t
  = Device { -- The `API.DeviceId` of this device.
             _identifier     :: MDynamic t API.DeviceId
             -- | The currently selected family (if any).
           , _selectedFamily :: MDynamic t (Family t)
           , _deviceType     :: Dynamic t DeviceType
           }


instance Reflex t => Default (Config t) where
  def = mempty

instance Reflex t => Semigroup (Config t) where
  a <> b = Config { _onSetDeviceType = leftmost [_onSetDeviceType a, _onSetDeviceType b]
                  , _onSelectFamily  = leftmost [_onSelectFamily a, _onSelectFamily b]
                  , _familyConfig = _familyConfig a <> _familyConfig b
                  }

instance Reflex t => Monoid (Config t) where
  mempty = Config never never mempty
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onSetDeviceType    <$> ev)
      <*> doSwitch never (_onSelectFamily     <$> ev)
      <*> flattenWith doSwitch (_familyConfig <$> ev)


-- Auto generated lenses:

class HasConfig a where
  config :: Lens' (a t) (Config t)

  onSetDeviceType :: Lens' (a t) (Event t API.DeviceType)
  onSetDeviceType = config . go
    where
      go :: Lens' (Config t) (Event t API.DeviceType)
      go f config' = (\onSetDeviceType' -> config' { _onSetDeviceType = onSetDeviceType' }) <$> f (_onSetDeviceType config')


  onSelectFamily :: Lens' (a t) (Event t API.FamilyId)
  onSelectFamily = config . go
    where
      go :: Lens' (Config t) (Event t API.FamilyId)
      go f config' = (\onSelectFamily' -> config' { _onSelectFamily = onSelectFamily' }) <$> f (_onSelectFamily config')


  familyConfig :: Lens' (a t) (Family.Config t)
  familyConfig = config . go
    where
      go :: Lens' (Config t) (Family.Config t)
      go f config' = (\familyConfig' -> config' { _familyConfig = familyConfig' }) <$> f (_familyConfig config')


instance HasConfig Config where
  config = id


class HasDevice a where
  device :: Lens' (a t) (Device t)

  identifier :: Lens' (a t) (MDynamic t API.DeviceId)
  identifier = device . go
    where
      go :: Lens' (Device t) (MDynamic t API.DeviceId)
      go f device' = (\identifier' -> device' { _identifier = identifier' }) <$> f (_identifier device')


  selectedFamily :: Lens' (a t) (Dynamic t (Maybe (Family t)))
  selectedFamily = device . go
    where
      go :: Lens' (Device t) (Dynamic t (Maybe (Family t)))
      go f device' = (\selectedFamily' -> device' { _selectedFamily = selectedFamily' }) <$> f (_selectedFamily device')


  deviceType :: Lens' (a t) (Dynamic t DeviceType)
  deviceType = device . go
    where
      go :: Lens' (Device t) (Dynamic t DeviceType)
      go f device' = (\deviceType' -> device' { _deviceType = deviceType' }) <$> f (_deviceType device')


instance HasDevice Device where
  device = id

