{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-|
Module      : Gonimo.Server.CachedDb
Description : Database with cache.
Copyright   : (c) Robert Klotzner, 2017

This is a wrapper around 'Gonimo.Server.Db.Db' which provides a data cache
for the database. This module takes care of keeping database and cache in sync.
-}
module Gonimo.Server.CachedDb ( -- * Types
                                -- ** Top level interface
                                Config(..)
                              , HasConfig(..)
                              , CachedDb(..)
                              , HasCachedDb(..)
                              -- ** Updates
                              , Update(..)
                              , UpdateRequest
                              , updateRequest
                              -- ** Deletes
                              , Delete(..)
                              , DeleteRequest
                              , deleteRequest
                              -- ** Raw db access
                              , Db.Command(..)
                              , Db.Result(..)
                              , Db.Response
                              , Db.ErrorResult
                              , Db.request
                              , Db.command
                              -- * Functions
                              , make
                              ) where


import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Reader
import qualified Database.Persist                as Db
import           Database.Persist.Sql            (SqlBackend)
import           Reflex
import           Reflex.Host.App

import           Gonimo.Lib.RequestResponse
import           Gonimo.Prelude
import           Gonimo.Server.Cache             (Model, ModelDump)
import qualified Gonimo.Server.Cache             as Cache
import           Gonimo.Server.CachedDb.Internal
import qualified Gonimo.Server.Db                as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error



-- | Make a 'CachedDb'.
--
--  Note this calls 'Db.make' which forks a thread in the background that is
--  never killed. This needs to be changed if usage is short lived!
make :: forall r t m. (Reflex t, MonadAppHost t m) => Config r t -> m (CachedDb r t)
make conf =  build $ \impl' -> do
    let
      dbUpdates = attachWith makeDbUpdates (impl'^.dbCache) (conf^.onUpdate)
      dbDeletes = fmap (map makeDbDelete) (conf^.onDelete)


      dbRequests = mconcat [ dbUpdates
                           , dbDeletes
                           , map (requester %~ (,id)) <$> conf ^. onDbCommand
                           ]

      -- We delay the cache write until the data is persisted in the database to
      -- ensure consistency, in case of gonimo-back gets killed for example.
      cacheUpdates = (^.requester . _2) <$> impl' ^. db . Db.onResponse

      cacheLoads = makeCacheLoads <$> impl' ^. db . Db.onResponse

    db' <- Db.make $ Db.Config { Db._serverConfig = conf ^. serverConfig
                               , Db._onRequest    = dbRequests
                               }

    cache' <- Cache.make $ Cache.Config { Cache._onLoadData = cacheLoads
                                        , Cache._onUpdate = cacheUpdates
                                        -- TODO: Implement Garbage collector!
                                        , Cache._onLoadModel = never
                                        }
    pure $ Impl { __cachedDb = CachedDb { _onResponse = (requester %~ fst ) <$> db' ^. Db.onResponse
                                        , _dbCache = cache'
                                        }
                , _db = db'
                }
  where
    build :: (Impl r t -> m (Impl r t)) -> m (CachedDb r t)
    build = fmap __cachedDb . mfix

makeCacheLoads :: Db.Response r -> ModelDump
makeCacheLoads = fromMaybe mempty . (^? Db.result . to Db.toModelDump)

makeDbUpdates :: Model -> [UpdateRequest r] -> [DbRequest r]
makeDbUpdates model' = map (makeDbUpdate model')

makeDbUpdate :: forall r. Model -> UpdateRequest r -> DbRequest r
makeDbUpdate model' (RequestResponse r upd@Update {..}) =
  let
    mT = model' ^? _updateTable . at _updateIndex . _Just . to _updatePerform

    c :: ReaderT SqlBackend IO ()
    c = Db.replace (toDb _updateIndex) . toDb =<< fromMaybeErr NotFound mT
  in
    RequestResponse (r, makeCacheUpdate upd) (Db.Write c)

makeDbDelete :: DeleteRequest r -> DbRequest r
makeDbDelete (RequestResponse r del@Delete {..}) =
  let
    c = Db.delete (toDb _deleteIndex)
  in
    RequestResponse (r, makeCacheDelete del) (Db.Write c)

makeCacheUpdate :: Update -> Model -> Model
makeCacheUpdate Update {..} = _updateTable . at _updateIndex . _Just %~ _updatePerform

makeCacheDelete :: Delete -> Model -> Model
makeCacheDelete Delete {..} = _deleteTable . at _deleteIndex .~ Nothing

