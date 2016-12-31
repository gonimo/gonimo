{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Gonimo.Server.Subscriber.Types where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Monoid
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import Debug.Trace (trace)

import           Gonimo.SocketAPI

type ReferenceCount = Int
type Revision = Int
type ResourceStatusMap = Map ServerRequest (TVar (RefCounted ResourceStatus))

data ResourceStatus = Modified Revision deriving (Eq, Show)

data RefCounted a = RefCounted {
  refCount :: ReferenceCount
, refValue :: a
}

instance Functor RefCounted where
  fmap f (RefCounted c v) = RefCounted c (f v)

data Subscriber = Subscriber {
  subState   :: !(TVar ResourceStatusMap)
, runLogging :: LogRunner
}

notifyChange :: Subscriber -> ServerRequest -> STM ()
notifyChange subscriber req' = do
  rMap <- readTVar (subState subscriber)
  case Map.lookup req' rMap of
    Nothing -> return ()
    Just refStatus -> do
      modifyTVar refStatus $ fmap (\(Modified n) -> Modified (n+1))

-- | Version of notify that lives in 'IO' - for your convenience.
notifyChangeIO :: Subscriber -> ServerRequest -> IO ()
notifyChangeIO sub req' = atomically $ notifyChange sub req'

-- | Subscribe to a ResourceStatus - it will be created when not present
subscribe :: ServerRequest -> Subscriber -> STM (TVar (RefCounted ResourceStatus))
subscribe p s = do
      states <- readTVar $ subState s
      let mState = Map.lookup p states
      case mState of
        Nothing -> do
           state <- newTVar $ RefCounted 1 (Modified 0)
           writeTVar (subState s) $ Map.insert p state states
           return state
        Just state -> do
           modifyTVar' state $ \s -> s { refCount = refCount s + 1}
           return state

-- | Unget a previously got ResourceState - make sure you match every call to subscribe with a call to unsubscribe!
unsubscribe :: ServerRequest -> TVar (RefCounted ResourceStatus) -> Subscriber -> STM ()
unsubscribe p tv s = do
  v <- (\a -> a { refCount = refCount a - 1}) <$> readTVar tv
  if refCount v == 0
    then modifyTVar' (subState s) (Map.delete p)
    else writeTVar tv v

type LogRunner = forall m a. MonadIO m => LoggingT m a -> m a
