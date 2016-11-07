-- | Device Session management.
--
--   A session as known in this module, simply tracks whether a given device is
--   currently online and whtether it acts as a baby station or not.

--   This module is meant to be imported qualified:
--   import Gonimo.Server.State.Session as Session
module Gonimo.Server.State.Session where


import           Control.Lens
import           Control.Monad             (unless, when, mzero, MonadPlus, guard)
import qualified Data.Map.Strict           as M
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Data.Monoid

import           Gonimo.Server.Db.Entities (DeviceId)
import           Gonimo.Server.Error      (ServerError (NoActiveSession, SessionInvalid),
                                           ToServerError, toServerError)
import           Gonimo.Server.Types      (DeviceType)
import           Gonimo.Server.State.Types (FamilyOnlineState, sessions, idCounter, SessionId(..))


-- | update might fail.
--
--   `NotFoundError` - You did not register before or got cleaned already
--   by the server, you have to re-register.
--   `AlreadyPresentError` : A session for this device is already present, but
--   session id does not match.
--   `NoUpdate` Not really an error, just saying that update did not really update anything, because there was no need.
data UpdateError = NotFoundError | AlreadyPresentError | NoUpdate

-- Needed for MonadPlus instance
instance Monoid UpdateError where
  mempty = NoUpdate
  mappend NoUpdate b = b
  mappend a _        = a

instance ToServerError UpdateError where
  toServerError NotFoundError = return NoActiveSession
  toServerError AlreadyPresentError = return SessionInvalid
  toServerError NoUpdate = mzero


-- | Register a session for a given device.
--
--   Any previous online session will simply be overridden. We steal the session.
register :: MonadState FamilyOnlineState m => DeviceId -> DeviceType -> m SessionId
register deviceId deviceType = do
  sessionId <- getNewSessionId
  sessions.at deviceId .= Just (sessionId, deviceType)
  return sessionId


-- | Update a given session - might fail see `UpdateError`
--
--   Returns: The old device type on update or mzero if new device type is same as old one.
update :: (MonadState FamilyOnlineState m, MonadError UpdateError m
          , MonadPlus m)
                => DeviceId -> SessionId -> DeviceType -> m DeviceType
update deviceId sessionId deviceType = do
  mFullSession   <- use $ sessions.at deviceId
  fullSession    <- maybe (throwError NotFoundError) return mFullSession
  let
    foundSession = fullSession^._1
    foundDevice  = fullSession^._2
  unless (foundSession == sessionId) $ throwError AlreadyPresentError

  guard (foundDevice /= deviceType)
  sessions.at deviceId .= Just (sessionId, deviceType)
  return foundDevice

list :: FamilyOnlineState ->  [(DeviceId, DeviceType)]
list = kickSessionId . getList
  where
    kickSessionId = over (mapped._2) (^._2)
    getList = (^.sessions.to M.toList)

-- | Delete your online session
--
--   If either device id or session id don't match - nothing happens.
delete :: (MonadState FamilyOnlineState m, MonadPlus m) => DeviceId -> SessionId -> m ()
delete deviceId sessionId = do
  mFoundSession <- gets $ (^? sessions.at deviceId._Just._1)
  guard (Just sessionId == mFoundSession)
  sessions.at deviceId .= Nothing


-- Internal functions ....

getNewSessionId :: MonadState FamilyOnlineState m => m SessionId
getNewSessionId = do
  newId <- use idCounter
  idCounter += 1
  pure $ SessionId newId


