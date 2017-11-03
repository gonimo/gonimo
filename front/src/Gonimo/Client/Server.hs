{-# LANGUAGE RecursiveDo #-}

module Gonimo.Client.Server ( Config
                            , Server
                            , HasConfig(..)
                            , HasServer(..)
                            , create
                            ) where

import Gonimo.SocketAPI (ServerRequest(..), ServerResponse)
import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Time
import Control.Lens
import qualified Data.Aeson as Aeson
import GHCJS.DOM.Types (MonadJSM)
import qualified Data.ByteString.Lazy as BL
import qualified Gonimo.Client.Reflex.Dom.WebSocket as WS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Control.Monad.Fix
import Data.Monoid
import Control.Monad.IO.Class
import Gonimo.Constants
import Data.Default

data Config t
  = Config { _configRequest :: Event t [ServerRequest]
           }


data Server t
  = Server { _open :: Event t ()
           , _response :: Event t ServerResponse
           , _closeRequested :: Event t ()
           , _close :: Event t ()
           }

class HasConfig a where
  config :: Lens' (a t) (Config t)

  configRequest :: Lens' (a t) (Event t [ServerRequest])
  configRequest = config . go
    where
      go :: Lens' (Config t) (Event t [ServerRequest])
      go f cfg' = (\configRequest' -> cfg' { _configRequest = configRequest' }) <$> f (_configRequest  cfg')

instance HasConfig Config where
  config = id

class HasServer a where
  server :: Lens' (a t) (Server t)

  open :: Lens' (a t) (Event t ())
  open = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\open' -> cfg' { _open = open' }) <$> f (_open  cfg')

  response :: Lens' (a t) (Event t ServerResponse)
  response = server . go
    where
      go :: Lens' (Server t) (Event t ServerResponse)
      go f cfg' = (\response' -> cfg' { _response = response' }) <$> f (_response  cfg')

  closeRequested :: Lens' (a t) (Event t ())
  closeRequested = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\closeRequested' -> cfg' { _closeRequested = closeRequested' }) <$> f (_closeRequested  cfg')

  close :: Lens' (a t) (Event t ())
  close = server . go
    where
      go :: Lens' (Server t) (Event t ())
      go f cfg' = (\close' -> cfg' { _close = close' }) <$> f (_close  cfg')

instance HasServer Server where
  server = id


instance Reflex t => Default (Config t) where
  def = Config never


create :: forall t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
                      , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                      )
          => Text -> Config t -> m (Server t)
create url conf = mdo
  let
    server' = Server { _open = ws^.WS.onOpen
                     , _response = decodedResponse
                     , _closeRequested = killConn
                     , _close = const () <$> ws^.WS.onClose
                     -- , _socket = ws
                     }

  (requests, killConn) <- handlePingPong (conf^.configRequest) decodedResponse

  let
    wsConfig :: WS.Config t
    wsConfig = def
               & WS.configOnSend .~ encodedRequests
               & WS.configOnClose .~ reqClose
               & WS.configCloseTimeout .~ Just 4 -- Try with 4 seconds.

    encodedRequests = fmap (T.decodeUtf8 . BL.toStrict . Aeson.encode) <$> requests

    decodedResponse = decodeResponse <$> ws^.WS.onReceive
    reqClose = const (WS.CloseParams 4000 "Server did not respond.") <$> killConn

  ws <- WS.create url wsConfig

  pure server'



decodeResponse :: Text -> ServerResponse
decodeResponse = fromMaybe (error "Decoding Server Response Failed!")
                 . Aeson.decodeStrict
                 . T.encodeUtf8


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
