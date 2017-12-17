{-|
Module      : Gonimo.Server.Clients
Description : Handle and serve clients.
Copyright   : (c) Robert Klotzner, 2017
This module implements the http/webSocket server for actually serving the
clients and making them part of the server FRP network.
-}
module Gonimo.Server.Clients ( -- * Types & Classes
                               Config(..)
                             , HasConfig(..)
                             , Clients(..)
                             , HasClients(..)
                              -- * Creation & initialization
                             , make
                             , serve
                             ) where


import           Control.Monad.Fix
import qualified Network.HTTP.Types.Status      as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets             as WS
import           Reflex.Host.App                as App

import           Gonimo.Prelude
import           Gonimo.Server.Authorize        (HasAuthorize (..))
import qualified Gonimo.Server.Authorize        as Authorize
import           Gonimo.Server.Clients.Internal
import qualified Gonimo.Server.Session          as Session
import qualified Gonimo.SocketAPI.APIVersion    as APIVersion




make :: forall t m. MonadAppHost t m => Config t -> m (Session.Config, Clients t)
make conf = build $ \impl' -> do
    (onReceived', fireOnReceived) <- newExternalEvent
    (onNewSession', fireOnNewSession) <- newExternalEvent
    (onRemovedClient', fireOnRemovedClient) <- newExternalEvent

    sessions' <- makeSessions impl'

    statuses' <- makeStatuses conf impl'

    let auth = Authorize.make
               $ Authorize.Config { Authorize.__clients = impl'^.clients
                                  , Authorize._cache    = conf^.cache
                                  , Authorize._onAuthorize = onReceived'
                                  }

    sendMessages impl' $ mconcat [ fmap (:[]) (auth^.onForbidden)
                                 , conf^.onSend
                                 ]


    pure $ Impl { __clients = Clients { _onReceived = auth^.onAuthorized
                                      , _onCreatedClient = fst <$> onNewSession'
                                      , _onRemovedClient = onRemovedClient'
                                      , _statuses = statuses'
                                      }
                , _onNewSession = onNewSession'
                , _sessionConfig = Session.Config { Session._serverConfig = conf^.serverConfig
                                                  , Session._receiveMessage = void . fireOnReceived
                                                  , Session._authenticated = void . fireOnNewSession
                                                  , Session._quit = void . fireOnRemovedClient
                                                  }
                , _sessions = sessions'
                }

  where
    build :: (Impl t -> m (Impl t)) -> m (Session.Config, Clients t)
    build = fmap (_sessionConfig &&& __clients) . mfix

-- | Serve our clients.
--
--   Incoming WebSocket requests are handled and sessions opened for each client.
--   Once the client authenticated it will be visible in 'Clients'.
serve :: Session.Config -> Wai.Application
serve conf req respond =
  let
    mVersion = (Wai.pathInfo req)^?_Cons._1.to APIVersion.fromText._Just

    wsResponse = do
      version <- mVersion
      Wai.websocketsApp WS.defaultConnectionOptions (Session.make conf version) req

    errorResponse :: Wai.Response
    errorResponse = if Wai.isWebSocketsReq req
                    then Wai.responseLBS (Http.Status 400 "No valid API version selected!") [] ""
                    else Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
  in
    respond $ fromMaybe errorResponse wsResponse


