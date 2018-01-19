{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Device entity
--   This module is intended to be imported qualified as Device
module Gonimo.Server.Db.Device where

import           Control.Monad                   (MonadPlus, guard)
import           Control.Monad.State.Class       as State
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Data.Text                       (Text)


import           Control.Monad
import           Control.Monad.Base              (MonadBase)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Data.Time                       (UTCTime)
import           Database.Persist.Class          (PersistEntity,
                                                  PersistEntityBackend)
import qualified Database.Persist.Class          as Db
import           Database.Persist.Sql            (SqlBackend)
import           Database.Persist                (Entity(..))

import           Gonimo.Database.Effects.Servant
import qualified Gonimo.Db.Entities              as Db
import           Gonimo.Server.Db.Internal       (UpdateT, updateRecord)
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error             (ServerError (NoSuchDevice, InvalidAuthToken))
import           Gonimo.SocketAPI.Types
import           Gonimo.Types (AuthToken)

type UpdateDeviceT m a = UpdateT Device m a

-- | Create a new device in the database
insert :: MonadIO m => Device -> ReaderT SqlBackend m DeviceId
insert = fmap fromDb . Db.insert . toDb

get :: (MonadBase IO m, MonadIO m) => DeviceId -> ReaderT SqlBackend m Device
get devId' = fmap fromDb . getErr (NoSuchDevice devId') . toDb $ devId'

getByAuthToken :: (MonadBase IO m, MonadIO m) => AuthToken -> ReaderT SqlBackend m (DeviceId, Device)
getByAuthToken token = do
    (Entity devId dev) <- getByAuthErr (Db.AuthTokenDevice token)
    pure (fromDb devId, fromDb dev)
  where
    getByAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.getBy

setName :: (MonadState Device m, MonadPlus m) => Text -> m ()
setName name = do
  prevName <- deviceName <$> State.get
  guard (prevName /= Just name)
  modify $ \ d -> d { deviceName = Just name }

setLastAccessed :: (MonadState Device m, MonadPlus m) => UTCTime -> m ()
setLastAccessed time = do
  prevTime <- deviceLastAccessed <$> State.get
  guard (prevTime /= time)
  modify $ \ d -> d { deviceLastAccessed = time }

-- | Update db entity as specified by the given UpdateDeviceT - on Nothing, no update occurs.
update :: ( PersistEntity (DbType Device), MonadIO m, MonadBase IO m
          , PersistEntityBackend (DbType Device) ~ SqlBackend
          ) 
          => DeviceId -> UpdateDeviceT (ReaderT SqlBackend m) a -> MaybeT (ReaderT SqlBackend m) a
update = updateRecord NoSuchDevice
