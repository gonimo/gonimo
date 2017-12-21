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
import qualified Gonimo.Server.Cache as Cache

data Config requester t
  = Config { _onUpdate :: Event t [(requester, Update)]
           , _onDelete :: Event t [(requester, Delete)]
             -- | Take care on this one, you are bypassing the cache if you use a 'Write' directly!
           , _onDbCommand :: Event t [(requester, Command)]
           , _serverConfig :: Server.Config
           }


data CachedDb requester t
  = CachedDb { _onDbResponse :: Event t (requester, Either ServerError Result)
             , _cache :: Cache t
             }

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
