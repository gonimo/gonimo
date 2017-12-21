{-|
Module      : Gonimo.Server.CachedDb
Description : Short description
Copyright   : (c) Robert Klotzner, 2017
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Gonimo.Server.CachedDb ( -- * Types
                              , CachedDb(..)
                              ) where



import Gonimo.Server.CachedDb.Internal



make :: forall a t m. MonadAppHost t m => Config a t -> m (CachedDb a t)
make conf =  do
  _cache <- Cache.make $ Cache.Config { _onLoadData = dataLoaded
                                      , _onUpdate = dataPersisted
                                      -- TODO: Implement Garbage collector!
                                      , _onLoadModel = never
                                      }
