{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Gonimo.Server.Clients.Internal
Description : Types and internal functions for "Gonimo.Server.Clients"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.Internal where

import Reflex
import Reflex.Host.App

import Gonimo.Prelude

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


make :: forall t m. MonadAppHost t m => Config t -> m (Session.Config, Clients t)
make conf = build $ \impl -> do
    (onReceived', fireOnReceived) <- newExternalEvent
    (onCreatedClient', fireOnCreatedClient) <- newExternalEvent
    (onRemovedClient', fireOnRemovedClient) <- newExternalEvent

    sessions' <- makeSessions impl

    sendMessage impl $ conf^.onSend

    pure $ Impl { __clients = Clients { _onReceived = onReceived'
                                      , _onCreatedClient = onCreatedClient'
                                      , _onRemovedClient = onRemovedClient'
                                      , _onlineStatus = onlineStatus'
                                      , _selectedFamily = selectedFamily'
                                      , _bySelectedFamily = bySelectedFamily'
                                      }
                , _sessionConfig = Session.Config { Session.serverConfig = config^.serverConfig
                                                  , Session._receiveMessage = fireOnReceived
                                                  , Session._authenticated = fireOnCreatedClient
                                                  , Session._quit = fireOnRemovedClient
                                                  }
                , _sessions = sessions'
                }

  where
    build :: Impl t -> m (Impl t) -> m (Session.Config, Clients t)
    build = (_sessionConfig &&& __clients) . mfix

-- | Serve our clients.
--
--   Incoming WebSocket requests are handled and sessions opened for each client.
--   Once the client authenticated it will be visible in 'Clients'.
serve :: Session.Config -> Wai.Application
serve config req respond =
  let
    mVersion = (pathInfo req)^?_Cons._Just._1.to APIVersion.fromText

    wsResponse = do
      version <- mVersion
      Wai.websocketsApp WS.defaultConnectionOptions (Session.make config version) req

    errorResponse :: Wai.Response
    errorResponse = if isWebSocketsReq req
                    then Wai.responseLBS (Http.Status 400 "No valid API version selected!") [] ""
                    else Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
  in
    respond $ fromMaybe errorResponse wsResponse

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
makeSessions impl = do
    sessions' <- foldp id Map.empty $ mergeWith (.) [ uncurry Map.insert <$> impl^.onCreatedClient
                                                    , Map.delete <$> impl^.onRemovedClient
                                                    ]
    let stealSession = second (const StoleSession) <$> impl^.onCreatedClient
    performEvent_ $ attachWith sendMessage' sessions' stealSession
    pure sessions'

-- | Handle onSend events by calling 'sendMessage''.
sendMessage :: MonadAppHost t m => Impl t -> Event t (DeviceId, ToClient) -> m ()
sendMessage impl onSend' = performEvent_ $ attachWith sendMessage' (impl^.sessions) onSend'

-- | Send a message asynchronously to a given device.
sendMessage' :: Map DeviceId Session -> (DeviceId, ToClient) -> IO ()
sendMessage' sessions' (destination, msg) = void . forkIO $ do
  let mSendMessage = sessions'^?at destination._Just.sendMessage
  sequence_ $ mSendMessage <$> pure msg

