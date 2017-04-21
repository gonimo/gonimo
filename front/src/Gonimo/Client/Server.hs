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
import Gonimo.Client.Reflex.Dom.WebSocket (WebSocketConfig(..), RawWebSocket(..), webSocket)
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


type Server t = RawWebSocket t ServerResponse

-- Generate our own lenses, because simle lenses are not good enough!
makeLenses ''WebSocketConfig
makeLenses ''RawWebSocket

server :: forall t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
                      , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                      )
          => Text -> ServerConfig t -> m (Server t)
server url config = do
  newReq <- handlePingPong (config^.webSocketConfig_send)
  let config' = config & webSocketConfig_send .~ newReq
  ws <- webSocket url
#ifdef DEVELOPMENT
        $ config' & webSocketConfig_send . mapped . mapped %~ T.decodeUtf8 . BL.toStrict . Aeson.encode
#else
        $ config' & webSocketConfig_send . mapped . mapped %~ BL.toStrict . Aeson.encode
#endif
  let ws' = ws & webSocket_recv . mapped %~ decodeResponse
  pure ws'



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
                  => Event t [ServerRequest] -> m (Event t [ServerRequest])
handlePingPong inRequest = do
  now <- liftIO $ getCurrentTime
  tick <- tickLossy webSocketPingInterval now
  let req = const [ReqPing] <$> tick
  let outRequest = req <> inRequest
  -- This is not such a good idea, killing the connection makes gonimo unusable on slow links instead of just slow, let's see how far we can get with plain TCP and ping pong to detect a broken link:
  -- Later on we could use a similar response time measurement to detect slow links and inform the user that a slow connection was detected and that gonimo might not work as expected.
  -- responseCount :: Dynamic t Int <- count $ server'^.webSocket_recv
  -- watchDog <- delay webSocketMaxRoundTrip $ pushAlways (\_ -> sample $ current responseCount) outRequest
  -- let killConn = push (\oldCount -> do
  --                         newCount <- sample $ current responseCount
  --                         if newCount > oldCount
  --                           then pure Nothing
  --                           else pure $ Just (1000, "Connection timed out - you got 7 seconds to respond!")
  --                     ) watchDog
  pure outRequest
