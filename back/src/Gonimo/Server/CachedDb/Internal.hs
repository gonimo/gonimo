{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Gonimo.Server.CachedDb.Internal
Description : Types and internal functions for "Gonimo.Server.CachedDb"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.CachedDb.Internal where


import Control.Lens
import Reflex
import qualified Database.Persist as Db


import Gonimo.Server.Db.IsDb
import  Gonimo.Server.Cache (Cache, Model)
import qualified Gonimo.Server.Cache as Cache
import  Gonimo.Server.Db (Db)
import qualified Gonimo.Server.Db as Db
import qualified Gonimo.Server.Config as Server
import Gonimo.Lib.RequestResponse

data Config r t
  = Config { _onUpdate :: Event t [UpdateRequest r]
           , _onDelete :: Event t [DeleteRequest r]
             -- | Take care on this one, you are bypassing the cache if you use a 'Write' directly!
           , _onDbCommand :: Event t [Db.Request r]
           , _serverConfig :: Server.Config
           }


data CachedDb r t
  = CachedDb { -- | On receiving a response the triggering request is executed
               --   and the cache will represent the changes in the next frame.
               _onResponse :: Event t (Db.Response r)
             , _dbCache :: Cache t
             }

data Impl r t
  = Impl { __cachedDb :: CachedDb r t
         , _db :: Db (r, Model -> Model) t
         }

type UpdateRequest r = RequestResponse r Update
type DeleteRequest r = RequestResponse r Update

-- | Internal DbRequest representation, for updating the model after the write succeeded.
type DbRequest r = Db.Request (r, Model -> Model)
-- | Internal DbResponse representation, for updating the model after the write succeded.
type DbResponse r = Db.Response (r, Model -> Model)


-- | Updates that will update the cache and the database.
--
--   'Update' always updates a single row in a table.
--
--   The model will be update in the following way:
--
-- @
--   model & (u ^.updateTable) . at (u ^. updateIndex) . Just %~ (u ^. updatePerform)
-- @
--
--   The database can be updated like this (pseudo code):
--
-- @ let
--     mt = model ^? (u ^. updateTable) . at (u ^. updateIndex) . _Just . to (u ^. updatePerform)
--   in
--     case mt of
--       Nothing -> error "blah"
--       Just t -> runDb conf $ replace (update ^. index . toDb) (toDb t)
-- @
data Update
  = forall table index m.
  ( At (m index table) -- ^ We need to be able to index the table
  , Ord index -- ^ For this the index needs Ord
  , IxValue (m index table) ~ index -- ^ Convenient alias
  , Index (m index table) ~ table -- ^ Convenient alias
  , IsDbType index -- ^ Ok index also needs to be a DbType
  , IsDbType table -- ^ Same goes for table
  , DbType index ~ Db.Key (DbType table) -- ^ And the key must match the table for db access.
  )
  => Update { -- | Get the table in the model
              _updateTable :: Lens' Cache.Model (m index table)
              -- | On which element of the table do we operate on?
            , _updateIndex :: index
              -- | Update the given entry
            , _updatePerform :: table -> table
            }

-- | A 'Delete' operation that will operate on the cache and the data base.
data Delete
  = forall table index m.
  ( At (m index table) -- ^ We need to be able to index the table
  , Ord index -- ^ For this the index needs Ord
  , IxValue (m index table) ~ index -- ^ Convenient alias
  , Index (m index table) ~ table -- ^ Convenient alias
  , IsDbType index -- ^ Ok index also needs to be a DbType
  )
  => Delete { -- | Get the table in the model
              _deleteTable :: Lens' Cache.Model (m index table)
              -- | On which element of the table do we operate on?
            , _deleteIndex :: index
            }

-- * Instances

instance Server.HasConfig (Config r t) where
  config = serverConfig


instance HasCachedDb Impl where
  cachedDb = _cachedDb

-- Auto generated lenses:


class HasConfig a where
  config :: Lens' (a r t) (Config r t)

  onUpdate :: Lens' (a r t) (Event t [UpdateRequest r])
  onUpdate = config . go
    where
      go :: Lens' (Config r t) (Event t [UpdateRequest r])
      go f config' = (\onUpdate' -> config' { _onUpdate = onUpdate' }) <$> f (_onUpdate config')


  onDelete :: Lens' (a r t) (Event t [DeleteRequest r])
  onDelete = config . go
    where
      go :: Lens' (Config r t) (Event t [DeleteRequest r])
      go f config' = (\onDelete' -> config' { _onDelete = onDelete' }) <$> f (_onDelete config')


  onDbCommand :: Lens' (a r t) (Event t [Db.Request r])
  onDbCommand = config . go
    where
      go :: Lens' (Config r t) (Event t [Db.Request r])
      go f config' = (\onDbCommand' -> config' { _onDbCommand = onDbCommand' }) <$> f (_onDbCommand config')


  serverConfig :: Lens' (a r t) Server.Config
  serverConfig = config . go
    where
      go :: Lens' (Config r t) Server.Config
      go f config' = (\serverConfig' -> config' { _serverConfig = serverConfig' }) <$> f (_serverConfig config')


instance HasConfig Config where
  config = id

class HasCachedDb a where
  cachedDb :: Lens' (a r t) (CachedDb r t)

  onResponse :: Lens' (a r t) (Event t (Db.Response r))
  onResponse = cachedDb . go
    where
      go :: Lens' (CachedDb r t) (Event t (Db.Response r))
      go f cachedDb' = (\onResponse' -> cachedDb' { _onResponse = onResponse' }) <$> f (_onResponse cachedDb')


  dbCache :: Lens' (a r t) (Cache t)
  dbCache = cachedDb . go
    where
      go :: Lens' (CachedDb r t) (Cache t)
      go f cachedDb' = (\cache' -> cachedDb' { _dbCache = cache' }) <$> f (_dbCache cachedDb')


instance HasCachedDb CachedDb where
  cachedDb = id

class HasImpl a where
  impl :: Lens' (a r t) (Impl r t)

  _cachedDb :: Lens' (a r t) (CachedDb r t)
  _cachedDb = impl . go
    where
      go :: Lens' (Impl r t) (CachedDb r t)
      go f impl' = (\_cachedDb' -> impl' { __cachedDb = _cachedDb' }) <$> f (__cachedDb impl')


  db :: Lens' (a r t) (Db (r, Model -> Model) t)
  db = impl . go
    where
      go :: Lens' (Impl r t) (Db (r, Model -> Model) t)
      go f impl' = (\db' -> impl' { _db = db' }) <$> f (_db impl')


instance HasImpl Impl where
  impl = id

