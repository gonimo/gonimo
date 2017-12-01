{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Gonimo.Server.Cache.Loader
Description : Ensures that some data is loaded in cache.
Copyright   : (c) Robert Klotzner, 2017
You can use this module for issuing cache loads and delaying some event until
the data is ready.
TODO: Add usage example.
-}
module Gonimo.Server.Cache.Loader ( -- * Types and classes
                                    Config(..)
                                  , Loader(..)
                                  , HasConfig(..)
                                  , HasLoader(..)
                                  , make
                                  ) where

import Reflex
import Reflex.Behavior
import Control.Lens
import Control.Monad.Fix
import           Data.Map (Map)
import qualified Data.Map as Map




{--
  Client requests some data ('Get'):
  Either family or account data needs to be fetched if not yet there.

-> load:
 -- Data there: Ready event - done
 -- Data not there:
    -- Issue load
    -- Queue request
    -- onload: send ready event
    -- Remove requests from queue.
--}

data Config t k request
  = Config {
      -- | E.g. for a Behavior map you can define '_isCached' like so:
      --
      -- @
      --   LoaderConfig { _isCached = flip Map.member <$> someMapBehavior
      --                }
      -- @
      _isCached :: Behavior t (k -> Bool)

      -- | Event from the cache manager, if this event is triggered it is
      --   assumed that the requested data can be found in the cache already.
    , _onLoaded :: Event t k

      -- | User event that should be delayed until the data specified by k is available in the cache.
    , _onCache :: Event t (k, request)
    }

data Loader t k request
  = Loader { -- | Load request to be handled by the cache manager.
             _onLoadRequest :: Event t k

             -- | Delayed input events.
             --
             --   When triggered the needed data is available in the cache (if
             --   the given key was valid in the first place.)
           , _onCached :: Event t [request]
           }

type Queues k request = Map k [request]

-- | Delays an event until the required data is cached.
--
--   When the delayed event triggers, either the data is in cache or it was not
--   found, result: If the data is not in the cache then, it wasn't in the
--   database either.
make :: forall m t k request c. (MonadHold t m, HasConfig c, Reflex t, MonadFix m, Eq k, Ord k)
     => c t k request -> m (Loader t k request)
make conf = do
    let
      inCache :: Event t ((k, request), Bool)
      inCache = attachWith checkCached (conf^.isCached) (conf^.onCache)

      needsLoad, alreadyLoaded :: Event t (k, request)
      needsLoad      = not `cached` inCache
      alreadyLoaded  = is  `cached` inCache


    queues <- foldp id Map.empty
      $ mergeWith (.) [ queueReq <$> needsLoad
                      , unqueueReqs <$> conf^.onLoaded
                      ]
    let
      readyRequests = attachWith getQueue queues (conf^.onLoaded)

      bornReadyRequests = (:[]) . snd <$> alreadyLoaded

      _onLoadRequest = fst <$> needsLoad
      _onCached = mconcat [ readyRequests
                          , bornReadyRequests
                          ]
    pure $ Loader { .. }
  where
    getQueue q k = q^.at k. non' _Empty

    checkCached :: (k -> Bool) -> (k, request) -> ((k, request), Bool)
    checkCached check entry@(key, _) = (entry, check key)

    cached :: forall a. (Bool -> Bool) -> Event t (a, Bool) -> Event t a
    cached f = fmap fst . ffilter (f . snd)

    is = id

    queueReq :: (k, request) -> Queues k request -> Queues k request
    queueReq (k, req) = at k . non' _Empty %~ (req:)

    unqueueReqs :: k -> Queues k request -> Queues k request
    unqueueReqs k = at k .~ Nothing

-- Lenses:

class HasConfig a where
  config :: Lens' (a t k request) (Config t k request)

  isCached :: Lens' (a t k request) (Behavior t (k -> Bool))
  isCached = config . go
    where
      go :: Lens' (Config t k request) (Behavior t (k -> Bool))
      go f config' = (\isCached' -> config' { _isCached = isCached' }) <$> f (_isCached config')


  onLoaded :: Lens' (a t k request) (Event t k)
  onLoaded = config . go
    where
      go :: Lens' (Config t k request) (Event t k)
      go f config' = (\onLoaded' -> config' { _onLoaded = onLoaded' }) <$> f (_onLoaded config')


  onCache :: Lens' (a t k request) (Event t (k, request))
  onCache = config . go
    where
      go :: Lens' (Config t k request) (Event t (k, request))
      go f config' = (\onCache' -> config' { _onCache = onCache' }) <$> f (_onCache config')


instance HasConfig Config where
  config = id

class HasLoader a where
  loader :: Lens' (a t k request) (Loader t k request)

  onLoadRequest :: Lens' (a t k request) (Event t k)
  onLoadRequest = loader . go
    where
      go :: Lens' (Loader t k request) (Event t k)
      go f loader' = (\onLoadRequest' -> loader' { _onLoadRequest = onLoadRequest' }) <$> f (_onLoadRequest loader')


  onCached :: Lens' (a t k request) (Event t [request])
  onCached = loader . go
    where
      go :: Lens' (Loader t k request) (Event t [request])
      go f loader' = (\onCached' -> loader' { _onCached = onCached' }) <$> f (_onCached loader')


instance HasLoader Loader where
  loader = id

