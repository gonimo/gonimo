{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-|
Module      : Gonimo.Server.Cache
Description : Database cache.
Copyright   : (c) Robert Klotzner, 2017

Database cache needed for consistency and performance. The database cannot be
kept in sync with the reflex network efficiently, so in order for the clients to
hava a consistent view on the data we need to cache the data, the clients hold in
memory.

This cache is based on models, it does not know nothing about views and what data
is actually needed by clients, external code should make use of 'onLoadData' for
ensuring needed data is cached.
-}
module Gonimo.Server.Cache ( -- * Types and classes
                             Config(..)
                           , HasConfig(..)
                           , Cache
                           , Model(..)
                           , HasModel(..)
                           , ModelDump(..)
                           , HasModelDump(..)
                             -- * Functions
                           , make
                           ) where

import Reflex
import Control.Lens
import Reflex.Behavior
import Control.Monad.Fix

import Gonimo.Server.Cache.Internal
import Gonimo.Server.Db.Internal (ModelDump(..), HasModelDump(..))


make :: (MonadFix m, MonadHold t m, Reflex t) => Config t -> m (Cache t)
make conf = do
  -- Order of events is important here! 'onLoadModel' erases all data so it has to be
  -- last in the chain, so it woll be executed first. Then we execute 'loadData'
  -- and finally we execute 'onUpdate' which then is guaranteed to operate on
  -- the most current data.
  foldp id emptyModel $ mergeWith (.) [ conf^.onUpdate
                                      , loadDump <$> conf^.onLoadData
                                      , const <$> conf^.onLoadModel
                                      ]

-- | Get the Account id of a given device.
-- getDeviceAccountId :: HasModel a => DeviceId -> a -> Maybe AccountId
-- getDeviceAccountId devId m = m ^? devices . at devId . _Just . to deviceAccountId
