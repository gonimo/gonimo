{-# LANGUAGE GADTs #-}
module Gonimo.Database.Effects.Servant where
-- Little helpers integrating db functions with servant:

import           Control.Monad              ((<=<))
import           Control.Monad.Catch        as X (MonadThrow (..))
import           Control.Monad.IO.Class     (MonadIO)
import           Database.Persist           (Entity, Key, Unique, getBy)
import           Database.Persist.Class     (PersistRecordBackend,
                                             PersistStoreRead,
                                             PersistUniqueRead, get)
import           Control.Monad.Trans.Reader (ReaderT)

import           Gonimo.Server.Error

type GetConstraint backend m a = (PersistStoreRead backend, MonadIO m, PersistRecordBackend a backend)

get404 :: (GetConstraint backend m a, MonadThrow m) => Key a -> ReaderT backend m a
get404 = getErr NotFound

getErr :: (GetConstraint backend m a, MonadThrow m)
          => ServerError -> Key a -> ReaderT backend m a
getErr err = serverErrOnNothing err <=< get

getBy404 :: (PersistUniqueRead backend, MonadIO m, PersistRecordBackend a backend, MonadThrow m)
            => Unique a -> ReaderT backend m (Entity a)
getBy404 = getByErr NotFound

getByErr :: (PersistUniqueRead backend, MonadIO m, PersistRecordBackend a backend, MonadThrow m)
            => ServerError -> Unique a -> ReaderT backend m (Entity a)
getByErr err = serverErrOnNothing err <=< getBy

serverErrOnNothing :: MonadThrow m
                      => ServerError -> Maybe a -> ReaderT backend m a
serverErrOnNothing err Nothing = throwM err
serverErrOnNothing _ (Just v)  = return v
