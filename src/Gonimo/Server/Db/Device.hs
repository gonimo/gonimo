-- | Functions for working with Device entity
--   This module is intended to be imported qualified as Device
module Gonimo.Server.Db.Device where

import           Control.Monad                   (MonadPlus, guard)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Data.Text                       (Text)

import           Gonimo.Server.Error             (ServerError (NoSuchDevice))

import           Control.Exception               (SomeException)
import           Control.Monad.Freer.Exception   (Exc)
import           Data.Time                       (UTCTime)
import           Database.Persist.Sql            (SqlBackend)
import           Gonimo.Database.Effects         (Database)
import           Gonimo.Server.Db.Entities       (DeviceId, Device(..))
import           Gonimo.Server.Db.Internal       (updateRecord, UpdateT)

type UpdateDeviceT m a = UpdateT Device m a

setName :: (MonadState Device m, MonadPlus m) => Text -> m ()
setName name = do
  prevName <- deviceName <$> get
  guard (prevName /= name)
  modify $ \ d -> d { deviceName = name }

setLastAccessed :: (MonadState Device m, MonadPlus m) => UTCTime -> m ()
setLastAccessed time = do
  prevTime <- deviceLastAccessed <$> get
  guard (prevTime /= time)
  modify $ \ d -> d { deviceLastAccessed = time }

-- | Update db entity as specified by the given UpdateDeviceT - on Nothing, no update occurs.
update :: DeviceId -> UpdateDeviceT (Eff '[Exc SomeException, Database SqlBackend]) a -> MaybeT (Eff '[Exc SomeException, Database SqlBackend]) a
update = updateRecord NoSuchDevice
