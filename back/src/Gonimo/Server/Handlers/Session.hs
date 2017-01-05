module Gonimo.Server.Handlers.Session where

import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Concurrent                   (threadDelay)
import           Data.Monoid
import           Data.Maybe                           (fromMaybe)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Db.Entities
import           Gonimo.Server.Effects
import           Gonimo.Server.Handlers.Auth.Internal
import           Gonimo.Server.State.Session          as Session
import           Gonimo.Server.State.Types            (ChannelMessage(SessionGotStolen))
import           Debug.Trace                          (trace)

import           Gonimo.Types
import           Servant.Subscriber                  (Event (ModifyEvent))
import qualified Gonimo.Server.Db.Family as Family
import qualified Gonimo.Server.Db.Device as Device
import           Utils.Constants                     (killSessionTimeout)
import qualified Control.Concurrent.Async            as Async

-- | A device registers itself as online.
--
--   With the returned session id a device can keep itself online by calls to sessionUpdateR or
--   make itself go offline with sessionDeleteR.
--
--   Only one session per device is allowed, with sessionRegisterR you can steal
--   a session. Any browser tab already online (triggering sessionUpdateR
--   periodically), will be set offline and the user will get an appropriate
--   error message.
sessionRegisterR :: (AuthReader m, MonadServer m)
                => FamilyId -> DeviceId -> OnlineDevice -> m ()
sessionRegisterR familyId deviceId onlineDevice = do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId
    now <- getCurrentTime
    mNotify <- runDb . runMaybeT $ do
      Device.update deviceId $ Device.setLastAccessed now
      pure . notify $ ReqGetDeviceInfo deviceId
    sequence_ mNotify
    runMaybeT $ do
      old <- MaybeT . updateFamilyEff familyId $ Session.register deviceId onlineDevice
      liftIO $ (old^.onlineDeviceSend) SessionGotStolen
    handleUpdate familyId deviceType

-- | Change device type (become a baby or stop being one)
setDeviceTypeR  :: (AuthReader m, MonadServer m)
               => FamilyId -> DeviceId -> SessionId -> DeviceType -> m ()
setDeviceTypeR familyId deviceId sessionId deviceType= do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId

    fmap (fromMaybe ()) . runMaybeT $ do
      _ :: DeviceType <- updateFamilyErrEff familyId
                         $ Session.setDeviceType deviceId sessionId deviceType
      lift $ handleUpdate familyId deviceType

-- | Tell the server that you are gone.
sessionDeleteR  :: (AuthReader m, MonadServer m)
               => FamilyId -> DeviceId -> m ()
sessionDeleteR familyId deviceId = do
  authorizeAuthData $ isFamilyMember familyId
  authorizeAuthData $ isDevice deviceId
  handleDelete familyId deviceId

sessionListDevicesR  :: (AuthReader m, MonadServer m)
                    => FamilyId -> m [(DeviceId, DeviceType)]
sessionListDevicesR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  Session.list <$> getFamilyEff familyId

-- Internal helpers:

-- Update last used baby names and send out notifications.
handleUpdate :: MonadServer m => FamilyId -> DeviceType -> m ()
handleUpdate familyId deviceType = do
  mNotifyFamily <- runDb . runMaybeT $ do
   fmap (fromMaybe ()) . runDb . runMaybeT $ do
    name <- toBabyName deviceType
    Family.update familyId (Family.pushBabyName name)
    pure . notify $ ReqGetFamily familyId
  sequence_ mNotifyFamily
  notify $ ReqGetOnlineDevices familyId

handleDelete :: MonadServer m => FamilyId -> DeviceId -> m ()
handleDelete familyId deviceId = do
  fmap (fromMaybe ()) . runMaybeT $ do
    mayUpdateFamilyEff familyId $ Session.delete deviceId
    lift . notify $ ReqGetOnlineDevices familyId
