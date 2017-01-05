module Gonimo.Server.Messenger where


import           Control.Lens
import           Control.Monad             (unless, mzero, MonadPlus, guard)
import qualified Data.Map.Strict           as M
import           Control.Monad.Error.Class
import           Control.Monad.State.Class

import           Gonimo.Db.Entities (DeviceId)
import           Gonimo.Server.Error      (ServerError (NoActiveSession, SessionInvalid),
                                           ToServerError, toServerError)
import           Gonimo.Types      (DeviceType)
import           Gonimo.Server.State.Types (Online, sessions, idCounter, SessionId(..))
import           Control.Monad.Extra (whenJust)


-- | update might fail.
--
--   `NotFoundError` - You did not register before or got cleaned already
--   by the server, you have to re-register.
--   `NoUpdate` Not really an error, just saying that update did not really update anything, because there was no need.
data UpdateError = NotFoundError |  NoUpdate

-- Needed for MonadPlus instance
instance Monoid UpdateError where
  mempty = NoUpdate
  mappend NoUpdate b = b
  mappend a _        = a

instance ToServerError UpdateError where
  toServerError NotFoundError = return NoActiveSession
  toServerError NoUpdate = mzero


-- | Register a receiver for a given device.
--
--   Any previous receiver will simply be overridden. We steal the session.
registerReceiver :: MonadState Messenger m => DeviceId -> Receiver -> m (Maybe Receiver)
registerReceiver deviceId receiver = do
  mOld <- use $ messengerReceivers.at deviceId
  messengerReceivers.at deviceId .= Just receiver
  whenJust receiver^.receiverFamily
    $ \familyId' -> messengerFamilies.at familyId' . non Set.empty %~ Set.insert deviceId
  return mOld


-- | Update a given session - might fail see `UpdateError`
--
--   Returns: The old device type on update or mzero if new device type is same as old one.
setDeviceType :: (MonadState Online m, MonadError UpdateError m
          , MonadPlus m) => DeviceId -> OnlineDevice -> m ()
setDeviceType deviceId onlineDevice = do
  mOld   <- use $ onlineDevices.at deviceId
  old    <- maybe (throwError NotFoundError) return mFullSession

  guard (old^.onlineDeviceType /= onlineDevice^.onlineDeviceType)
  onlineDevices.at deviceId._Just.onlineDeviceType .= onlineDevice^.onlineDeviceType

list :: Online ->  [(DeviceId, DeviceType)]
list = kickSend . getList
  where
    kickSend = over (mapped._2) onlineDeviceType
    getList = (^.onlineDevices.to M.toList)

-- | Delete your online session
--
--   If device id doesn't match - nothing happens.
delete :: (MonadState Online m, MonadPlus m) => DeviceId -> m ()
delete deviceId = sessions.at deviceId .= Nothing
