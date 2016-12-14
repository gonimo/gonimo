module Gonimo.Server.Handlers.Session where

import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Concurrent                   (threadDelay)
import           Data.Monoid
import           Data.Maybe                           (fromMaybe)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects
import           Gonimo.Server.Handlers.Auth.Internal
import           Gonimo.Server.State.Session          as Session
import           Gonimo.Server.State.Types            (SessionId)
import           Debug.Trace                          (trace)

import           Gonimo.Server.Types
import           Servant.Subscriber                  (Event (ModifyEvent))
import qualified Gonimo.Server.Db.Family as Family
import qualified Gonimo.Server.Db.Device as Device
import           Utils.Constants                     (killSessionTimeout)
import qualified Control.Concurrent.Async            as Async
import           Gonimo.Server.State.SessionThread   as Thread

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
                => FamilyId -> DeviceId -> DeviceType -> m SessionId
sessionRegisterR familyId deviceId deviceType = do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId
    now <- getCurrentTime
    mNotify <- runDb . runMaybeT $ do
      Device.update deviceId $ Device.setLastAccessed now
      pure $ notify ModifyEvent getDeviceInfosEndpoint (\f -> f familyId)
    sequence_ mNotify
    sessionId <- updateFamilyEff familyId $ Session.register deviceId deviceType
    handleUpdate familyId deviceType
    handleRegisterKiller familyId deviceId sessionId
    pure $ trace ("Registered session: " <> show sessionId) sessionId

-- | Update session (you can change the device type any time)
--
--   Even if you don't change the device type, you have to trigger this handler
--   at least every 30 seconds in order to stay online.
--
--   If another browser tab created a new session, this handler will throw
--   Error: `SessionInvalid`, you can use this error to detect whether the
--   user opened the app in more than one browser tab.
--
--   You will get error `NoActiveSession` if there is no session at all - you
--   came too late and have to call sessionRegisterR again!
sessionUpdateR  :: (AuthReader m, MonadServer m)
               => FamilyId -> DeviceId -> SessionId -> DeviceType -> m ()
sessionUpdateR familyId deviceId sessionId deviceType= do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId

    fmap (fromMaybe ()) . runMaybeT $ do
      _ :: DeviceType <- updateFamilyErrEff familyId
                         $ Session.update deviceId sessionId deviceType
      lift $ handleUpdate familyId deviceType
    handleRegisterKiller familyId deviceId sessionId

-- | Tell the server that you are gone.
sessionDeleteR  :: (AuthReader m, MonadServer m)
               => FamilyId -> DeviceId -> SessionId ->  m ()
sessionDeleteR familyId deviceId sessionId = do
  authorizeAuthData $ isFamilyMember familyId
  authorizeAuthData $ isDevice deviceId
  handleDelete familyId deviceId sessionId

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
    name <- toBabyName deviceType
    Family.update familyId (Family.pushBabyName name)
    pure $ notify ModifyEvent getFamilyEndpoint (\f -> f familyId)
  sequence_ mNotifyFamily
  notify ModifyEvent listDevicesEndpoint (\f -> f familyId)

handleDelete :: MonadServer m => FamilyId -> DeviceId -> SessionId -> m ()
handleDelete familyId deviceId sessionId = do
  fmap (fromMaybe ()) . trace ("Deleting session: " <> show sessionId) . runMaybeT $ do
    mayUpdateFamilyEff familyId $ Session.delete deviceId sessionId
    lift $ notify ModifyEvent listDevicesEndpoint (\f -> f familyId)
    -- Only remove killer if session was still valid, otherwise we kill the wrong killer!
    old <- MaybeT . updateFamilyEff familyId $ Thread.remove deviceId
    liftIO $ Async.cancel old


deleteThread :: MonadServer m => FamilyId -> DeviceId -> SessionId -> m ()
deleteThread familyId deviceId sessionId = do
  liftIO $ threadDelay killSessionTimeout
  handleDelete familyId deviceId sessionId


handleRegisterKiller :: MonadServer m => FamilyId -> DeviceId -> SessionId -> m ()
handleRegisterKiller familyId deviceId sessionId = fmap (fromMaybe ()) .  runMaybeT $ do
  killer <- lift . async $ deleteThread familyId deviceId sessionId
  old <- MaybeT . updateFamilyEff familyId $ Thread.register deviceId killer
  liftIO $ Async.cancel old
