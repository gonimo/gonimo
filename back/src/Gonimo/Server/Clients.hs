{-|
Module      : Gonimo.Server.Clients
Description : Handle and serve clients.
Copyright   : (c) Robert Klotzner, 2017
This module implements the http/webSocket server for actually serving the
clients and making them part of the server FRP network.
-}
module Gonimo.Server.Clients ( -- * Types
                               Clients(..)
                             ) where


import Reflex as Reflex
import Reflex.Host.App as App
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types.Status        as Http
import Control.Monad.Fix



import Gonimo.Prelude
import Gonimo.Server.Session (Session, HasSession)
import qualified Gonimo.Server.Session as Session
import qualified Gonimo.Server.Config as Server
import Gonimo.SocketAPI
import Gonimo.SocketAPI.Model
import           Gonimo.SocketAPI.APIVersion (APIVersion)
import qualified Gonimo.SocketAPI.APIVersion as APIVersion
import Gonimo.Server.Clients.Internal

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
                , _sessionConfig = Session.Config { Session._serverConfig = config^.serverConfig
                                                  , Session._receiveMessage = fireOnReceived
                                                  , Session._authenticated = fireOnCreatedClient
                                                  , Session._quit = fireOnRemovedClient
                                                  }
                , _sessions = sessions'
                }

  where
    build :: m (Impl t) -> m (Session.Config, Clients t)
    build = fmap (_sessionConfig &&& __clients) . mfix

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


