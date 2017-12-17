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
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Network.HTTP.Types.Status      as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets             as WS
import           Reflex                         as Reflex
import           Reflex.Host.App                as App



import           Gonimo.Prelude
import           Gonimo.Server.Authorize        (Authorize, HasAuthorize (..))
import qualified Gonimo.Server.Authorize        as Authorize
import           Gonimo.Server.Clients.Internal
import qualified Gonimo.Server.Config           as Server
import           Gonimo.Server.Session          (HasSession, Session)
import qualified Gonimo.Server.Session          as Session
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.APIVersion    (APIVersion)
import qualified Gonimo.SocketAPI.APIVersion    as APIVersion
import           Gonimo.SocketAPI.Model



make :: forall t m. MonadAppHost t m => Config t -> m (Session.Config, Clients t)
make conf = build $ \impl -> do
    (onReceived', fireOnReceived) <- newExternalEvent
    (onNewSession', fireOnNewSession) <- newExternalEvent
    (onRemovedClient', fireOnRemovedClient) <- newExternalEvent

    sessions' <- makeSessions impl

    statuses' <- makeStatuses impl

    let auth = Authorize.make
               $ Authorize.Config { Authorize.__clients = impl^.clients
                                  , Authorize._cache    = conf^.cache
                                  , Authorize._onAuthorize = onReceived'
                                  }

    sendMessages impl $ mconcat [ fmap (:[]) (auth^.onForbidden)
                                , conf^.onSend
                                ]


    pure $ Impl { __clients = Clients { _onReceived = auth^.onAuthorized
                                      , _onCreatedClient = fst <$> onNewSession'
                                      , _onRemovedClient = onRemovedClient'
                                      , _statuses = statuses'
                                      }
                , _onNewSession = onNewSession'
                , _sessionConfig = Session.Config { Session._serverConfig = config^.serverConfig
                                                  , Session._receiveMessage = void . fireOnReceived
                                                  , Session._authenticated = void . fireOnNewSession
                                                  , Session._quit = void . fireOnRemovedClient
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


