{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Gonimo.Server.CachedDb
Description : Short description
Copyright   : (c) Robert Klotzner, 2017
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Gonimo.Server.CachedDb ( -- * Types
                                Config(..)
                              , HasConfig(..)
                              , CachedDb(..)
                              , HasCachedDb(..)
                              , make
                              ) where


import Reflex
import Reflex.Host.App
import qualified Database.Persist as Db
import Control.Lens
import Control.Monad.Fix

import Gonimo.Server.Db.IsDb
import  Gonimo.Server.Cache (Cache, Model, ModelDump)
import qualified Gonimo.Server.Cache as Cache
import  Gonimo.Server.Db (Db)
import qualified Gonimo.Server.Db as Db
import qualified Gonimo.Server.Config as Server
import Gonimo.Lib.RequestResponse
import Gonimo.Server.Error

import Gonimo.Server.CachedDb.Internal



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
makeCacheLoads = (^. Db.result . to Db.toModelDump)

makeDbUpdates :: Model -> [UpdateRequest r] -> [DbRequest r]
makeDbUpdates model' = map (makeDbUpdate model')

makeDbUpdate :: Model -> UpdateRequest r -> DbRequest r
makeDbUpdate model' (RequestResponse r upd@Update {..}) =
  let
    mT = cache ^? _updateTable . at _updateIndex . _Just . to _updatePerform
    c = Db.replace (toDb _updateIndex) . toDb =<< fromMaybeErr NotFound mT
  in
    RequestResponse (r, makeCacheUpdate upd) (Write c)

makeDbDelete :: DeleteRequest r -> DbRequest r
makeDbDelete (RequestResponse r del@Delete {..}) =
  let
    c = Db.delete (toDb _deleteIndex)
  in
    RequestResponse (r, makeCacheDelete del) (Write c)

makeCacheUpdate :: Update -> Model -> Model
makeCacheUpdate Update {..} = _updateTable . at _updateIndex . _Just %~ _updatePerform

makeCacheDelete :: Delete -> Model -> Model
makeCacheDelete Delete {..} = _deleteTable . at _deleteIndex .~ Nothing

