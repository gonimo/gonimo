{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Gonimo.Server.Clients.Internal
Description : Types and internal functions for "Gonimo.Server.Clients"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.Internal where

import Reflex as Reflex
import Reflex.Host.App as App
import           Data.Map (Map)
import qualified Data.Map as Map
import Reflex.Behavior
import Control.Concurrent



import Gonimo.Prelude
import Gonimo.Server.Session (Session)
import qualified Gonimo.Server.Session as Session
import qualified Gonimo.Server.Config as Server
import Gonimo.SocketAPI
import Gonimo.SocketAPI.Model



data Config t
  = Config { _serverConfig :: Server.Config
           , _onSend :: Event t (DeviceId, ToClient)
           }

data Clients t
  = Clients { _onReceived       :: Event t (DeviceId, FromClient)
              -- | A device came online:
            , _onCreatedClient  :: Event t (DeviceId, Session)
              -- | A device went offline:
            , _onRemovedClient  :: Event t DeviceId
            , _onlineStatus     :: Behavior t (Map DeviceId DeviceStatus)
            , _selectedFamily   :: Behavior t (Map DeviceId FamilyId)
            , _bySelectedFamily :: Behavior t (Map FamilyId [DeviceId]) -- ^ Needed for routing messages to concerned devices.
            -- For notifying devices that they have been kicked out from a family they don't have currently selected,
            -- We just look up the kicked device in clients and notify it.
            -- Therefore with _bySelectedFamily the event route has all needed information for routing all events to concerend parties.
            }

-- | Sampled data, to be aquired with 'sampleAll'.
data Sampled
  = Sampled { _sampledOnlineStatus     :: Map DeviceId DeviceStatus
            , _sampledSelectedFamily   :: Map DeviceId FamilyId
            , _sampledBySelectedFamily :: Map FamilyId [DeviceId]
            }

data Impl t
  = Impl { __clients      :: Clients t
         , _sessionConfig :: Session.Config
         , _sessions      :: Behavior t (Map DeviceId Session)
         }



-- | Sample the whole cache for your convenience!
sampleAll :: forall t m. (Reflex t, MonadHold t m) => Clients t -> m Sampled
sampleAll clients' = do
  _sampledOnlineStatus     <- sample $ clients'^.onlineStatus
  _sampledSelectedFamily   <- sample $ clients'^.selectedFamily
  _sampledBySelectedFamily <- sample $ clients'^.bySelectedFamily
  pure $ Sampled {..}



-- | Create new sessions/ delete obsolete ones.
--
-- And also send SessionStolen messages to devices in case their session just got stolen.
makeSessions :: MonadAppHost t m => Impl t -> m (Behavior t (Map DeviceId Session))
makeSessions impl' = do
    sessions' <- foldp id Map.empty $ mergeWith (.) [ uncurry Map.insert <$> impl'^.onCreatedClient
                                                    , Map.delete <$> impl'^.onRemovedClient
                                                    ]
    let stealSession = second (const StoleSession) <$> impl'^.onCreatedClient
    App.performEvent_ $ attachWith sendMessage' sessions' stealSession
    pure sessions'

-- | Handle onSend events by calling 'sendMessage''.
sendMessage :: MonadAppHost t m => Impl t -> Event t (DeviceId, ToClient) -> m ()
sendMessage impl' onSend' = App.performEvent_ $ attachWith sendMessage' (impl'^.sessions) onSend'

-- | Send a message asynchronously to a given device.
sendMessage' :: MonadIO m => Map DeviceId Session -> (DeviceId, ToClient) -> m ()
sendMessage' sessions' (destination, msg) = liftIO . void . forkIO $ do
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


  onSend :: Lens' (a t) (Event t (DeviceId, ToClient))
  onSend = config . go
    where
      go :: Lens' (Config t) (Event t (DeviceId, ToClient))
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


  onCreatedClient :: Lens' (a t) (Event t (DeviceId, Session))
  onCreatedClient = clients . go
    where
      go :: Lens' (Clients t) (Event t (DeviceId, Session))
      go f clients' = (\onCreatedClient' -> clients' { _onCreatedClient = onCreatedClient' }) <$> f (_onCreatedClient clients')


  onRemovedClient :: Lens' (a t) (Event t DeviceId)
  onRemovedClient = clients . go
    where
      go :: Lens' (Clients t) (Event t DeviceId)
      go f clients' = (\onRemovedClient' -> clients' { _onRemovedClient = onRemovedClient' }) <$> f (_onRemovedClient clients')


  onlineStatus :: Lens' (a t) (Behavior t (Map DeviceId DeviceStatus))
  onlineStatus = clients . go
    where
      go :: Lens' (Clients t) (Behavior t (Map DeviceId DeviceStatus))
      go f clients' = (\onlineStatus' -> clients' { _onlineStatus = onlineStatus' }) <$> f (_onlineStatus clients')


  selectedFamily :: Lens' (a t) (Behavior t (Map DeviceId FamilyId))
  selectedFamily = clients . go
    where
      go :: Lens' (Clients t) (Behavior t (Map DeviceId FamilyId))
      go f clients' = (\selectedFamily' -> clients' { _selectedFamily = selectedFamily' }) <$> f (_selectedFamily clients')


  bySelectedFamily :: Lens' (a t) (Behavior t (Map FamilyId [DeviceId]))
  bySelectedFamily = clients . go
    where
      go :: Lens' (Clients t) (Behavior t (Map FamilyId [DeviceId]))
      go f clients' = (\bySelectedFamily' -> clients' { _bySelectedFamily = bySelectedFamily' }) <$> f (_bySelectedFamily clients')


instance HasClients Clients where
  clients = id

class HasSampled a where
  sampled :: Lens' a Sampled

  sampledOnlineStatus :: Lens' a (Map DeviceId DeviceStatus)
  sampledOnlineStatus = sampled . go
    where
      go :: Lens' Sampled (Map DeviceId DeviceStatus)
      go f sampled' = (\sampledOnlineStatus' -> sampled' { _sampledOnlineStatus = sampledOnlineStatus' }) <$> f (_sampledOnlineStatus sampled')


  sampledSelectedFamily :: Lens' a (Map DeviceId FamilyId)
  sampledSelectedFamily = sampled . go
    where
      go :: Lens' Sampled (Map DeviceId FamilyId)
      go f sampled' = (\sampledSelectedFamily' -> sampled' { _sampledSelectedFamily = sampledSelectedFamily' }) <$> f (_sampledSelectedFamily sampled')


  sampledBySelectedFamily :: Lens' a (Map FamilyId [DeviceId])
  sampledBySelectedFamily = sampled . go
    where
      go :: Lens' Sampled (Map FamilyId [DeviceId])
      go f sampled' = (\sampledBySelectedFamily' -> sampled' { _sampledBySelectedFamily = sampledBySelectedFamily' }) <$> f (_sampledBySelectedFamily sampled')


instance HasSampled Sampled where
  sampled = id

class HasImpl a where
  impl :: Lens' (a t) (Impl t)

  _clients :: Lens' (a t) (Clients t)
  _clients = impl . go
    where
      go :: Lens' (Impl t) (Clients t)
      go f impl' = (\_clients' -> impl' { __clients = _clients' }) <$> f (__clients impl')


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

