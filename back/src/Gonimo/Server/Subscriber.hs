{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.Subscriber where


import           Control.Lens
import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict               as Map
import           Data.Foldable                 (traverse_)
import           Data.Set                      ((\\), Set)
import qualified Data.Set                      as Set

import           Gonimo.Server.Subscriber.Types
import           Gonimo.SocketAPI (ServerRequest)

makeSubscriber :: STM Subscriber
makeSubscriber = do
  state' <- newTVar Map.empty
  pure $ Subscriber { subState = state'
                    }

makeClient :: Subscriber -> STM Client
makeClient subscriber' = do
  ms <- newTVar Map.empty
  return $ Client subscriber' ms

processRequest :: Client -> Set ServerRequest -> STM ()
processRequest c req = do
  oldMonitors <- readTVar (monitors c)
  let oldSet = Set.fromList . Map.keys $ oldMonitors
  let removeSet = oldSet \\ req
  let addSet = req \\ oldSet
  traverse_ (removeRequest c) . Set.toList $ removeSet
  traverse_ (addRequest c) . Set.toList $ addSet


notifyChange :: Subscriber -> ServerRequest -> STM ()
notifyChange subscriber' req' = do
  rMap <- readTVar (subState subscriber')
  case Map.lookup req' rMap of
    Nothing -> return ()
    Just refStatus -> do
      modifyTVar refStatus $ fmap (\(Modified n) -> Modified (n+1))

-- | Version of notify that lives in 'IO' - for your convenience.
notifyChangeIO :: Subscriber -> ServerRequest -> IO ()
notifyChangeIO sub req' = atomically $ notifyChange sub req'

-- | Subscribe to a ResourceStatus - it will be created when not present
subscribe :: ServerRequest -> Subscriber -> STM (TVar (RefCounted ResourceStatus))
subscribe p sub = do
      states <- readTVar $ subState sub
      let mState = Map.lookup p states
      case mState of
        Nothing -> do
           state <- newTVar $ RefCounted 1 (Modified 0)
           writeTVar (subState sub) $ Map.insert p state states
           return state
        Just state -> do
           modifyTVar' state $ \s -> s { refCount = refCount s + 1}
           return state

-- | Unget a previously got ResourceState - make sure you match every call to subscribe with a call to unsubscribe!
unsubscribe :: ServerRequest -> TVar (RefCounted ResourceStatus) -> Subscriber -> STM ()
unsubscribe p tv sub = do
  v <- (\a -> a { refCount = refCount a - 1}) <$> readTVar tv
  if refCount v == 0
    then modifyTVar' (subState sub) (Map.delete p)
    else writeTVar tv v


snapshotOld :: Snapshot -> Maybe ResourceStatus
snapshotOld = oldStatus . fullMonitor

toSnapshot :: StatusMonitor -> STM Snapshot
toSnapshot mon = do
  current <- readTVar $ monitor mon
  return Snapshot {
    snapshotCurrent = refValue current
  , fullMonitor = mon
  }

snapshotRequest :: Snapshot -> ServerRequest
snapshotRequest = request . fullMonitor

cleanup :: MonadIO m => Client ->  m ()
cleanup c = liftIO . atomically $ do
    ms <- readTVar (monitors c)
    mapM_ (unsubscribeMonitor (subscriber c)) ms

addRequest :: Client -> ServerRequest -> STM ()
addRequest c req = do
    let sub  = subscriber c
    tState <- subscribe req sub
    modifyTVar' (monitors c) $ at req .~ Just (StatusMonitor req tState Nothing)

-- | Remove a Request, also unsubscribes from subscriber and deletes our monitor
--   if it was the last Request for the given request.
removeRequest :: Client -> ServerRequest -> STM ()
removeRequest c req = do
    let sub = subscriber c
    monitors' <- readTVar (monitors c)
    traverse_ (unsubscribeMonitor sub) $ monitors'^.at req
    modifyTVar' (monitors c) $ at req .~ Nothing

-- | Does not remove the monitor - use removeRequest if you want this!
unsubscribeMonitor :: Subscriber -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    req = request m
    mon = monitor m
  in
    unsubscribe req mon sub


runMonitor :: MonadIO m => Client -> (ServerRequest -> m ()) -> m ()
runMonitor c handleRequest = forever $ do
    changes <- liftIO . atomically $ monitorChanges c
    mapM_ (handleRequest . fst) $ changes

monitorChanges :: Client -> STM [(ServerRequest, ResourceStatus)]
monitorChanges c = do
  snapshots <- mapM toSnapshot . Map.elems =<< readTVar (monitors c)
  let result = getChanges snapshots
  if null result
    then retry
    else do
      let newMonitors = monitorsFromList . updateMonitors $ snapshots
      writeTVar (monitors c) newMonitors
      return result


getChanges :: [Snapshot] -> [(ServerRequest, ResourceStatus)]
getChanges = map toChangeReport . filter monitorChanged

monitorChanged :: Snapshot -> Bool
monitorChanged m = Just (snapshotCurrent m) /= snapshotOld m

toChangeReport :: Snapshot -> (ServerRequest, ResourceStatus)
toChangeReport m = (snapshotRequest m, snapshotCurrent m)

updateMonitors :: [Snapshot] -> [StatusMonitor]
updateMonitors = map updateOldStatus

updateOldStatus :: Snapshot -> StatusMonitor
updateOldStatus m = (fullMonitor m) {
    oldStatus = Just $ snapshotCurrent m
  }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    reqs = map (request) ms
    assList = zip reqs ms
  in
    Map.fromList assList
