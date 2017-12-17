{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE GADTs       #-}
{-|
Module      : Gonimo.Server.Clients.Internal
Description : Types and internal functions for "Gonimo.Server.Clients"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.Internal where

import           Control.Concurrent
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Reflex                             as Reflex
import           Reflex.Behavior
import           Reflex.Host.App                    as App


import           Gonimo.Prelude
import           Gonimo.Server.Cache                (Cache, Model, devices)
import           Gonimo.Server.Clients.ClientStatus as Client
import qualified Gonimo.Server.Config               as Server
import           Gonimo.Server.Session              (Session)
import qualified Gonimo.Server.Session              as Session
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Model
import qualified Gonimo.Server.Cache.Devices        as Devices

data Config t
  = Config { _serverConfig :: Server.Config
           , _cache        :: Cache t
           , _onSend       :: Event t [(DeviceId, ToClient)]
           }

data Clients t
  = Clients {
              -- | A message was received.
              _onReceived      :: Event t (DeviceId, FromClient)
              -- | A device came online.
            , _onCreatedClient :: Event t DeviceId
              -- | A device went offline.
            , _onRemovedClient :: Event t DeviceId
            , _statuses        :: Behavior t ClientStatuses
            }

data Impl t
  = Impl { __clients      :: Clients t
         , _onNewSession  :: Event t (DeviceId, Session)
         , _sessionConfig :: Session.Config
         , _sessions      :: Behavior t (Map DeviceId Session)
         }

-- | Build client status map based on create/remove events and received status updates.
makeStatuses :: forall t m. MonadAppHost t m => Config t -> Impl t -> m (Behavior t ClientStatuses)
makeStatuses conf impl' = do
    foldp id emptyStatuses $ mergeWith (.) [ newStatus    <$> impl'^.onCreatedClient
                                           , removeStatus <$> impl'^.onRemovedClient
                                           , push updateStatus $ impl'^.onReceived
                                           ]
  where
    emptyStatuses = Client.makeStatuses Map.empty

    newStatus :: DeviceId -> ClientStatuses -> ClientStatuses
    newStatus devId = at devId .~ Just def

    removeStatus :: DeviceId -> ClientStatuses -> ClientStatuses
    removeStatus devId = at devId .~ Nothing

    -- updateStatus :: [(DeviceId, ToClient)] -> PushM t (Maybe (ClientStatuses -> ClientStatuses))
    -- updateStatus msgs = runMaybeT $ do
    --   model' <- lift . sample $ conf^.cache
    --   let updates = mapMaybe (updateStatusSingle model') msgs
    --   guard (not . null $ updates)
    --   pure $ foldr (.) id updates
    updateStatus :: (DeviceId, FromClient) -> PushM t (Maybe (ClientStatuses -> ClientStatuses))
    updateStatus msg = updateStatus' <$> sample (conf^.cache) <*> pure msg

    updateStatus' :: Model -> (DeviceId, FromClient) -> Maybe (ClientStatuses -> ClientStatuses)
    updateStatus' model' (_, msg)
      = case msg of
          UpdateServer (OnChangedDeviceStatus devId fid newStatus')
            -> pure $ at devId .~ Just (ClientStatus newStatus' (Just fid))
          UpdateServer (OnRemovedFamilyMember fid aid)
            -> do
            let byAccountId = Devices.byAccountId $ model'^.devices
            devices' <- byAccountId ^. at aid

            pure $ \statuses' ->
              let
                byFamilyId' = Client.byFamilyId statuses'
                familyDevices' = byFamilyId' ^. at fid . _Just
                hitDevices = Set.toList $ Set.intersection devices' familyDevices'
                removeFamily hitDevice = at hitDevice . _Just . clientFamily .~ Nothing

                removeFamilies = foldr (.) id . map removeFamily $ hitDevices
              in
                statuses' & removeFamilies
          _ -> mzero


-- | Create new sessions/ delete obsolete ones.
--
-- And also send SessionStolen messages to devices in case their session just got stolen.
makeSessions :: MonadAppHost t m => Impl t -> m (Behavior t (Map DeviceId Session))
makeSessions impl' = do
    sessions' <- foldp id Map.empty $ mergeWith (.) [ uncurry Map.insert <$> impl'^.onNewSession
                                                    , Map.delete <$> impl'^.onRemovedClient
                                                    ]
    let stealSession = (, StoleSession) <$> impl'^.onCreatedClient
    App.performEvent_ $ attachWith sendMessageAsync sessions' stealSession
    pure sessions'

-- | Handle onSend events by calling 'sendMessage''.
sendMessages :: MonadAppHost t m => Impl t -> Event t [(DeviceId, ToClient)] -> m ()
sendMessages impl' onSend'
  = App.performEvent_ $ attachWith sendMessagesAsync (impl'^.sessions) onSend'

-- | Send multiple messages asynchronously.
sendMessagesAsync :: MonadIO m => Map DeviceId Session -> [(DeviceId, ToClient)] -> m ()
sendMessagesAsync sessions'
  = liftIO . void . forkIO . traverse_ (sendMessage' sessions')

sendMessageAsync :: MonadIO m => Map DeviceId Session -> (DeviceId, ToClient) -> m ()
sendMessageAsync sessions' toClient' = liftIO . void . forkIO $ sendMessage' sessions' toClient'



-- | Send a message asynchronously to a given device.
-- | Helper function for sending messages.
--
--   This function would block the reflex network until the message got sent, so you should in general not use it directly. Use 'sendMessageAsync' or 'sendMessagesAsync'
sendMessage' :: Map DeviceId Session -> (DeviceId, ToClient) -> IO ()
sendMessage' sessions' (destination, msg) = do
  let
    mSendMessage :: Maybe (ToClient -> IO ())
    mSendMessage = sessions'^?at destination._Just.Session.sendMessage
  sequence_ $ mSendMessage <*> pure msg


instance HasClients Impl where
  clients = _clients

-- Lenses:

class HasConfig a where
  config :: Lens' (a t) (Config t)

  serverConfig :: Lens' (a t) Server.Config
  serverConfig = config . go
    where
      go :: Lens' (Config t) Server.Config
      go f config' = (\serverConfig' -> config' { _serverConfig = serverConfig' }) <$> f (_serverConfig config')


  cache :: Lens' (a t) (Cache t)
  cache = config . go
    where
      go :: Lens' (Config t) (Cache t)
      go f config' = (\cache' -> config' { _cache = cache' }) <$> f (_cache config')


  onSend :: Lens' (a t) (Event t [ (DeviceId, ToClient) ])
  onSend = config . go
    where
      go :: Lens' (Config t) (Event t [ (DeviceId, ToClient) ])
      go f config' = (\onSend' -> config' { _onSend = onSend' }) <$> f (_onSend config')


instance HasConfig Config where
  config = id

class HasClients a where
  clients :: Lens' (a t) (Clients t)

  onReceived :: Lens' (a t) (Event t (DeviceId, FromClient))
  onReceived = clients . go
    where
      go :: Lens' (Clients t) (Event t (DeviceId, FromClient))
      go f clients' = (\onReceived' -> clients' { _onReceived = onReceived' }) <$> f (_onReceived clients')


  onCreatedClient :: Lens' (a t) (Event t DeviceId)
  onCreatedClient = clients . go
    where
      go :: Lens' (Clients t) (Event t DeviceId)
      go f clients' = (\onCreatedClient' -> clients' { _onCreatedClient = onCreatedClient' }) <$> f (_onCreatedClient clients')


  onRemovedClient :: Lens' (a t) (Event t DeviceId)
  onRemovedClient = clients . go
    where
      go :: Lens' (Clients t) (Event t DeviceId)
      go f clients' = (\onRemovedClient' -> clients' { _onRemovedClient = onRemovedClient' }) <$> f (_onRemovedClient clients')


  statuses :: Lens' (a t) (Behavior t ClientStatuses)
  statuses = clients . go
    where
      go :: Lens' (Clients t) (Behavior t ClientStatuses)
      go f clients' = (\statuses' -> clients' { _statuses = statuses' }) <$> f (_statuses clients')


instance HasClients Clients where
  clients = id

class HasImpl a where
  impl :: Lens' (a t) (Impl t)

  _clients :: Lens' (a t) (Clients t)
  _clients = impl . go
    where
      go :: Lens' (Impl t) (Clients t)
      go f impl' = (\_clients' -> impl' { __clients = _clients' }) <$> f (__clients impl')


  onNewSession :: Lens' (a t) (Event t (DeviceId, Session))
  onNewSession = impl . go
    where
      go :: Lens' (Impl t) (Event t (DeviceId, Session))
      go f impl' = (\onNewSession' -> impl' { _onNewSession = onNewSession' }) <$> f (_onNewSession impl')


  sessionConfig :: Lens' (a t) Session.Config
  sessionConfig = impl . go
    where
      go :: Lens' (Impl t) Session.Config
      go f impl' = (\sessionConfig' -> impl' { _sessionConfig = sessionConfig' }) <$> f (_sessionConfig impl')


  sessions :: Lens' (a t) (Behavior t (Map DeviceId Session))
  sessions = impl . go
    where
      go :: Lens' (Impl t) (Behavior t (Map DeviceId Session))
      go f impl' = (\sessions' -> impl' { _sessions = sessions' }) <$> f (_sessions impl')


instance HasImpl Impl where
  impl = id

