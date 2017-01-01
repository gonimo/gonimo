{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Server.Subscriber where


import           Control.Concurrent.Async
import           Control.Lens
import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception             (SomeException, displayException)
import           Control.Exception.Lifted      (finally, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger          (MonadLogger, logDebug)
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.IORef                    (IORef, writeIORef)
import           Data.Map                      (Map)
import qualified Data.Map.Strict               as Map
import           Data.Monoid                   ((<>))
import           Data.Foldable                 (traverse_)
import           Data.Set                      (Set, (\\))
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection as WS
import Debug.Trace (trace)

import           Gonimo.Server.Subscriber.Types

type ClientMonitors = Map ServerRequest StatusMonitor

data Client = Client {
    subscriber      :: !Subscriber
  , monitors        :: !(TVar ClientMonitors)
  }

data StatusMonitor = StatusMonitor {
  request     :: !ServerRequest
, monitor     :: !(TVar (RefCounted ResourceStatus))
, oldStatus   :: !(Maybe ResourceStatus) -- Nothing when added so we get a notification in any case.
}

data Snapshot = Snapshot {
  snapshotCurrent :: ResourceStatus
, fullMonitor     :: StatusMonitor
}

makeClient :: Subscriber -> STM Client
makeClient subscriber = do
  ms <- newTVar Map.empty
  return Client subscriber ms

processRequest :: Client -> [ServerRequest] -> STM ()
processRequest c req = do
  oldMonitors <- readTVar (monitors c)
  let reqSet = Set.fromList req
  let oldSet = Set.fromList . Map.keys . monitors $ oldMonitors
  let removeSet = oldSet \\ reqSet
  let addSet = reqSet \\ oldSet
  traverse (removeRequest c) . toList $ removeSet
  traverse (addRequest c) . toList $ addSet


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

fromWebSocket :: Subscriber api -> IORef (IO ()) -> WS.Connection -> STM (Client)
fromWebSocket sub myRef c = do
  ms <- newTVar Map.empty
  closeVar <- newTVar (return ())
  return Client {
    subscriber = sub
  , monitors = ms
  , readRequest = do
      msg <- WS.receiveDataMessage c
      case msg of
        WS.Text bs  -> return $ decode bs
        WS.Binary _ -> error "Sorry - binary connections currently unsupported!"
  , writeResponse = sendDataMessage c . WS.Text . encode
  , pongCommandRef = myRef
  , closeCommandRef = closeVar
  }

-- run :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, Backend backend)
--        => backend -> Client -> m ()
-- run b c = do
--   let
--     sub     = subscriber c
--     work    = liftIO $ race_ (runMonitor b c) (handleRequests b c)
--   r <- try $ finally work cleanup
--   case r of
--     Left e -> $logDebug $ T.pack $ displayException (e :: SomeException)
--     Right _ -> return ()

run :: (MonadBaseControl IO m, MonadIO m)
       => Client -> m ()
cleanup c = do
  close <- liftIO . atomically $ do
    ms <- readTVar (monitors c)
    mapM_ (unsubscribeMonitor (subscriber c)) ms
    readTVar (closeCommandRef c)
  liftIO close

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
unsubscribeMonitor :: Subscriber api -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    req = request m
    mon = monitor m
  in
    unsubscribe req mon sub


runMonitor :: MonadIO m => Client -> (ServerRequest -> m ()) -> m ()
runMonitor c handleRequest = forever $ do
    changes <- atomically $ monitorChanges c
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
updateMonitors = map updateOldStatus . filter ((/= S.Deleted) . snapshotCurrent)

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
