{-# LANGUAGE RecursiveDo #-}

module Gonimo.Client.Server ( Config(..)
                            , Server(..)
                            , HasConfig(..)
                            , HasServer(..)
                            , Model
                            , HasModel
                            , make
                            ) where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Default
import           Reflex
import           Reflex.Dom.Class
import           Data.Time.Clock
import           GHCJS.DOM.Types           (MonadJSM)
import qualified Reflex.Dom.WebSocket      as WS

import           Gonimo.SocketAPI          (ServerRequest (..), ServerResponse)
import           Gonimo.Client.Environment (Environment, HasEnvironment)
import qualified Gonimo.Client.Environment as Environment
import           Gonimo.Client.Prelude


data Config t
  = Config { _onRequest :: Event t [ServerRequest]
           } deriving (Generic)


data Server t
  = Server { -- | Connection is opened - we are not authenticated yet.
             _onOpen           :: Event t ()

             -- | Responses received from the server.
           , _onResponse       :: Event t ServerResponse

             -- | We requested a connection close.
             --
             --   This usually happens on ping timeouts. You can use this event
             --   for informing the user about connection problems.
           , _onCloseRequested :: Event t ()

             -- | Connection actually got closed.
           , _onClose          :: Event t ()

           }

-- | Simple data type fulfilling our 'HasModel' constraint.
type Model t = Environment

-- | Our dependencies
type HasModel model = HasEnvironment model

instance Reflex t => Default (Config t) where
  def = mempty

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = memptydefault
  mappend = mappenddefault

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config <$> doSwitch never (_onRequest <$> ev)

make :: forall model c t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
                      , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                      , HasConfig c, HasModel model
                      )
          => model t -> c t -> m (Server t)
make model conf = mdo
  let
    server' = Server { _onOpen = ws^.WS.webSocket_open
                     , _onResponse = recv
                     , _onCloseRequested = killConn
                     , _onClose = const () <$> ws^.WS.webSocket_close
                     }

  (requests, killConn) <- handlePingPong (conf^.onRequest) recv

  let
    wsConfig :: WS.WebSocketConfig t ServerRequest
    wsConfig = def
               & WS.webSocketConfig_send .~ requests
               & WS.webSocketConfig_close .~ reqClose
               & WS.webSocketConfig_reconnect .~ True

    reqClose = const (4000, "Server did not respond.") <$> killConn

  ws <- WS.jsonWebSocket (model^.Environment.backendWSURL) wsConfig

  let recv = fromMaybe (error "Decoding Server Response Failed!") <$> ws^.WS.webSocket_recv

  pure server'



-- We need to handle ping/pong at the application level so the client can detect a broken connection.
-- Otherwise if the client has no data to send, but just waits for data (baby station), a broken connection will never be detected and the baby is unreachable - this is bad!

-- Example: Connection broken for a minute - server closes TCP/IP connection in
-- the meantime, connection is back up again - Client won't get any requests
-- because it still holds on to the dead connection. By sending a ping message
-- periodically, the death will be detected (within minutes - see TCP timeout
-- behavour) and a close followed by a reconnection can take place.

-- See also: https://blog.stephencleary.com/2009/05/detection-of-half-open-dropped.html
-- TCP timeout behaviour: http://www.pcvr.nl/tcpip/tcp_time.htm
handlePingPong :: forall t m. ( MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m)
                              , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                              )
                  => Event t [ServerRequest] -> Event t ServerResponse -> m (Event t [ServerRequest], Event t ())
handlePingPong inRequest inResponse = do
  now <- liftIO $ getCurrentTime
  tick <- tickLossy webSocketPingInterval now
  let req = const [ReqPing] <$> tick
  shallKill <- hold False $ leftmost [ const False <$> inResponse
                                     , const True <$> tick
                                     ]
  let killConn = fmap (const ()) . ffilter id . tag shallKill $ tick
  let outRequest = req <> inRequest
  pure (outRequest, killConn)

-- Auto generated lens stuff:
class HasConfig a where
  config :: Lens' (a t) (Config t)

  onRequest :: Lens' (a t) (Event t [ServerRequest])
  onRequest = config . go
    where
      go :: Lens' (Config t) (Event t [ServerRequest])
      go f cfg' = (\onRequest' -> cfg' { _onRequest = onRequest' }) <$> f (_onRequest  cfg')

instance HasConfig Config where
  config = id

class HasServer a where
  server :: Lens' (a t) (Server t)

  onOpen :: Lens' (a t) (Event t ())
  onOpen = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\onOpen' -> cfg' { _onOpen = onOpen' }) <$> f (_onOpen  cfg')

  onResponse :: Lens' (a t) (Event t ServerResponse)
  onResponse = server . go
    where
      go :: Lens' (Server t) (Event t ServerResponse)
      go f cfg' = (\onResponse' -> cfg' { _onResponse = onResponse' }) <$> f (_onResponse  cfg')

  onCloseRequested :: Lens' (a t) (Event t ())
  onCloseRequested = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\onCloseRequested' -> cfg' { _onCloseRequested = onCloseRequested' }) <$> f (_onCloseRequested  cfg')

  onClose :: Lens' (a t) (Event t ())
  onClose = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\onClose' -> cfg' { _onClose = onClose' }) <$> f (_onClose  cfg')

instance HasServer Server where
  server = id


























-- Old server code (reflex.dom websockets):
-- server :: forall t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
--                       , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
--                       )
--           => Text -> ServerConfig t -> m (Server t)
-- server url config = mdo
--   (newReq, killConn) <- handlePingPong (config^.webSocketConfig_send) (ws'^.webSocket_recv)
--   let config' = config & webSocketConfig_send .~ newReq
--                        & webSocketConfig_close .~ (const (4000, "Server did not respond.") <$> killConn)
--   ws <- webSocket url
-- #ifdef DEVELOPMENT
--         $ config' & webSocketConfig_send . mapped . mapped %~ T.decodeUtf8 . BL.toStrict . Aeson.encode
-- #else
--         $ config' & webSocketConfig_send . mapped . mapped %~ BL.toStrict . Aeson.encode
-- #endif
--   let ws' = ws & webSocket_recv . mapped %~ decodeResponse
--   pure $ Server ws' killConn
