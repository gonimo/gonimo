{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
module Gonimo.Client.Server where

import Gonimo.SocketAPI (ServerRequest(..), ServerResponse)
import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Time
import Control.Lens
import qualified Data.Aeson as Aeson
import GHCJS.DOM.Types (MonadJSM)
import qualified Data.ByteString.Lazy as BL
import Reflex.Dom.WebSocket (WebSocketConfig(..), RawWebSocket(..), webSocket)
import Data.ByteString (ByteString)
import Data.Text (Text)
#ifdef DEVELOPMENT
import qualified Data.Text.Encoding as T
#endif
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Control.Monad.Fix
import Data.Monoid
import Control.Monad.IO.Class
import Gonimo.Constants

type ServerConfig t = WebSocketConfig t ServerRequest


data Server t
  = Server { _socket :: RawWebSocket t ServerResponse
           -- After 30 seconds we attempt to close the connection, unfortunately
           -- it can take a significant amount of time until it really gets
           -- closed, but we would like to inform the user about the problem
           -- sooner. Therefore we provide the closeRequested event here:
           , _closeRequested :: Event t ()
           }

makeLenses ''Server
-- Generate our own lenses, because simple lenses are not good enough!
makeLenses ''WebSocketConfig
makeLenses ''RawWebSocket


server :: forall t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
                      , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                      )
          => Text -> ServerConfig t -> m (Server t)
server url config = mdo
  (newReq, killConn) <- handlePingPong (config^.webSocketConfig_send) (ws'^.webSocket_recv)
  let config' = config & webSocketConfig_send .~ newReq
                       & webSocketConfig_close .~ (const (4000, "Server did not respond.") <$> killConn)
  ws <- webSocket url
#ifdef DEVELOPMENT
        $ config' & webSocketConfig_send . mapped . mapped %~ T.decodeUtf8 . BL.toStrict . Aeson.encode
#else
        $ config' & webSocketConfig_send . mapped . mapped %~ BL.toStrict . Aeson.encode
#endif
  let ws' = ws & webSocket_recv . mapped %~ decodeResponse
  pure $ Server ws' killConn



decodeResponse :: ByteString -> ServerResponse
decodeResponse = fromMaybe (error "Decoding Server Response Failed!") . Aeson.decodeStrict


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
  killValue <- hold Nothing $ leftmost [ const Nothing <$> inResponse
                                       , const (Just ()) <$> tick
                                       ]
  -- If killValue is (Just ()) on a tick event - then we need to kill the
  -- connection. (Server needed more than webSocketPingInterval for a response.)
  let killConn = push (const $ sample killValue) tick
  let outRequest = req <> inRequest
  pure (outRequest, killConn)
