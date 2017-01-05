module Gonimo.Server.Handlers.Messenger where

import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid
import           Data.Maybe                           (fromMaybe)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Db.Entities
import           Gonimo.Server.Effects
import           Gonimo.Server.Handlers.Auth.Internal
import           Gonimo.Server.Messenger
import           Debug.Trace                          (trace)

import           Gonimo.Types
import qualified Gonimo.Server.Db.Family as Family
import qualified Gonimo.Server.Db.Device as Device

-- | A device registers itself (happens at authentication)
--
--   Afterwards you can use switchFamilyR for becoming online in a family.
registerReceiverR :: (AuthReader m, MonadServer m)
                =>  DeviceId -> (Message -> IO ()) -> m ()
registerReceiverR deviceId receiver = do
    authorizeAuthData $ isDevice deviceId

    now <- getCurrentTime
    mNotify <- runDb . runMaybeT $ do
      Device.update deviceId $ Device.setLastAccessed now
      pure . notify $ ReqGetDeviceInfo deviceId
    sequence_ mNotify

    runMaybeT $ do
      messenger <- getMessenger
      old <- atomically $ registerReceiverSTM messenger deviceId receiver
      liftIO $ (old^.onlineDeviceSend) MessageSessionGotStolen
    handleUpdate familyId deviceType

-- | Change device type (become a baby or stop being one)
setDeviceTypeR  :: (AuthReader m, MonadServer m)
               => DeviceId -> DeviceType -> m ()
setDeviceTypeR deviceId deviceType= do
    authorizeAuthData $ isDevice deviceId

    messenger <- getMessenger
    atomically $ setDeviceTypeSTM messenger deviceId deviceType

    notify $ ReqGetReceivers familyId

-- | Tell the server that you are gone.
deleteReceiverR  :: (AuthReader m, MonadServer m)
               =>  DeviceId -> m ()
deleteReceiverR deviceId = do
  authorizeAuthData $ isDevice deviceId

  messenger <- getMessenger
  atomically $ deleteReceiverSTM messenger deviceId

  notify $ ReqGetReceivers familyId

getOnlineDevicesR  :: (AuthReader m, MonadServer m)
                    => FamilyId -> m [(DeviceId, DeviceType)]
getOnlineDevicesR familyId = do
  authorizeAuthData $ isFamilyMember familyId

  messenger <- getMessenger
  atomically $ getOnlineDevicesSTM messenger familyId

switchFamilyR  :: (AuthReader m, MonadServer m)
                    => DeviceId -> FamilyId -> m [(DeviceId, DeviceType)]
switchFamilyR deviceId familyId = do
  authorizeAuthData $ isDevice deviceId
  authorizeAuthData $ isFamilyMember familyId

  messenger <- getMessenger
  atomically $ switchFamilySTM messenger deviceId familyId

-- Update last used baby names and send out notifications on change.
-- TODO: Does not really belong here!
putBabyNameR :: MonadServer m => FamilyId -> Text -> m ()
putBabyNameR familyId babyName = do
  authorizeAuthData $ isFamilyMember familyId

  mNotifyFamily <- runDb . runMaybeT $ do
    Family.update familyId (Family.pushBabyName babyName)
    pure . notify $ ReqGetFamily familyId
  sequence_ mNotifyFamily
