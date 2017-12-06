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
            , _onCreatedClient  :: Event t DeviceId
              -- | A device went offline:
            , _onRemovedClient  :: Event t DeviceId
            , _onlineStatus     :: Behavior t (Map DeviceId DeviceStatus)
            , _selectedFamily   :: Behavior t (Map DeviceId FamilyId)
            , _bySelectedFamily :: Behavior t (Map FamilyId DeviceId) -- ^ Needed for routing messages to concerned devices.
            -- For notifying devices that they have been kicked out from a family they don't have currently selected,
            -- We just look up the kicked device in clients and notify it.
            -- Therefore with _bySelectedFamily the event route has all needed information for routing all events to concerend parties.
            }

data Impl t
  = Impl { __clients      :: !(Clients t)
         , _sessionConfig :: Session.Config
         , _sessions      :: !(Map DeviceId Session)
         }


make :: forall t m. MonadAppHost t m => Config t -> m (Session.Config, Clients t)
make config = build $ do
    (onReceived', fireOnReceived) <- newExternalEvent
    (onCreatedClient', fireOnCreatedClient) <- newExternalEvent
    (onRemovedClient', fireOnRemovedClient) <- newExternalEvent

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
    build :: ReaderT (Impl t) m (Impl t) -> m (Session.Config, Clients t)
    build = (_sessionConfig &&& __clients) . mfix . runReaderT




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
