{-# LANGUAGE GADTs #-}
module Gonimo.Database.Effects.Servant where
-- Little helpers integrating db functions with servant:

import           Control.Monad                 ((<=<))
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Base             (MonadBase)
import           Database.Persist              (Entity, Key, Unique, getBy)
import           Database.Persist.Class        (PersistStore, get, PersistEntityBackend, PersistEntity, PersistUnique)
import           Gonimo.Server.Error
import           Control.Monad.Trans.Reader      (ReaderT)

type GetConstraint backend m a = (PersistStore backend, MonadIO m, backend ~ PersistEntityBackend a, PersistEntity a)

get404 :: (GetConstraint backend m a, MonadBase IO m) => Key a -> ReaderT backend m a
get404 = getErr NotFound

getErr :: (GetConstraint backend m a, MonadBase IO m)
          => ServerError -> Key a -> ReaderT backend m a
getErr err = serverErrOnNothing err <=< get

getBy404 :: (PersistUnique backend, MonadIO m, backend ~ PersistEntityBackend a, PersistEntity a, MonadBase IO m)
            => Unique a -> ReaderT backend m (Entity a)
getBy404 = getByErr NotFound

getByErr :: (PersistUnique backend, MonadIO m, backend ~ PersistEntityBackend a, PersistEntity a, MonadBase IO m)
            => ServerError -> Unique a -> ReaderT backend m (Entity a)
getByErr err = serverErrOnNothing err <=< getBy

serverErrOnNothing :: (PersistStore backend, MonadIO m, MonadBase IO m)
                      => ServerError -> Maybe a -> ReaderT backend m a
serverErrOnNothing err Nothing = throwServer err
serverErrOnNothing _ (Just v) = return v
