{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Device entity
--   This module is intended to be imported qualified as Device
module Gonimo.Server.Db.Device where

import           Control.Monad                   (MonadPlus, guard)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Data.Text                       (Text)

import           Gonimo.Server.Error             (ServerError (NoSuchDevice))

import           Data.Time                       (UTCTime)
import           Gonimo.Server.Db.Entities       (DeviceId, Device(..))
import           Gonimo.Server.Db.Internal       (updateRecord, UpdateT)
import           Control.Monad.Base              (MonadBase)
import           Control.Monad.IO.Class          (MonadIO)
import           Database.Persist.Class          (PersistEntity,
                                                  PersistEntityBackend,
                                                  PersistStore)
import           Control.Monad.Trans.Reader      (ReaderT (..))

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
update :: (PersistEntity Device, backend ~ PersistEntityBackend Device
          , MonadIO m, MonadBase IO m, PersistStore backend)
          => DeviceId -> UpdateDeviceT (ReaderT backend m) a -> MaybeT (ReaderT backend m) a
update = updateRecord NoSuchDevice
