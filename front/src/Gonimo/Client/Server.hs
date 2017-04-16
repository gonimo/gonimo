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

type ServerConfig t = WebSocketConfig t ServerRequest


type Server t = RawWebSocket t ServerResponse

maxResponseTime :: NominalDiffTime
maxResponseTime = 7

-- Generate our own lenses, because simle lenses are not good enough!
makeLenses ''WebSocketConfig
makeLenses ''RawWebSocket

server :: forall t m. ( MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
                      , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                      )
          => Text -> ServerConfig t -> m (Server t)
server url config = mdo
  let config' = config & webSocketConfig_send .~ newReq
                       & webSocketConfig_close .~ closeConn
  ws <- webSocket url
#ifdef DEVELOPMENT
        $ config' & webSocketConfig_send . mapped . mapped %~ T.decodeUtf8 . BL.toStrict . Aeson.encode
#else
        $ config' & webSocketConfig_send . mapped . mapped %~ BL.toStrict . Aeson.encode
#endif
  let ws' = ws & webSocket_recv . mapped %~ decodeResponse
  (newReq, closeConn) <- handlePingPong (config^.webSocketConfig_send) ws'
  pure ws'



decodeResponse :: ByteString -> ServerResponse
decodeResponse = fromMaybe (error "Decoding Server Response Failed!") . Aeson.decodeStrict


handlePingPong :: forall t m. ( MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m)
                              , HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m
                              )
                  => Event t [ServerRequest] -> Server t -> m (Event t [ServerRequest], Event t (Word, Text))
handlePingPong inRequest server' = do
  now <- liftIO $ getCurrentTime
  tick <- tickLossy 30 now
  let req = const [ReqPing] <$> tick
  let outRequest = req <> inRequest
  responseCount :: Dynamic t Int <- count $ server'^.webSocket_recv
  watchDog <- delay maxResponseTime $ pushAlways (\_ -> sample $ current responseCount) outRequest
  let killConn = push (\oldCount -> do
                          newCount <- sample $ current responseCount
                          if newCount > oldCount
                            then pure Nothing
                            else pure $ Just (1000, "Connection timed out - you got 7 seconds to respond!")
                      ) watchDog
  pure (outRequest, killConn)
  
