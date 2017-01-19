{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Gonimo.Client.Server where

import Gonimo.SocketAPI (ServerRequest, ServerResponse)
import Reflex
import Reflex.Class
import Reflex.Dom.Class
import Control.Lens
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
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

type ServerConfig t = WebSocketConfig t ServerRequest


type Server t = RawWebSocket t ServerResponse

-- Generate our own lenses, because simle lenses are not good enough!
makeLenses ''WebSocketConfig
makeLenses ''RawWebSocket

server :: forall t m. (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => Text -> ServerConfig t -> m (Server t)
server url config = do
  ws <- webSocket url
#ifdef DEVELOPMENT
        $ config & webSocketConfig_send . mapped . mapped %~ T.decodeUtf8 . BL.toStrict . Aeson.encode
#else
        $ config & webSocketConfig_send . mapped . mapped %~ BL.toStrict . Aeson.encode
#endif
  pure $ ws & webSocket_recv . mapped %~ decodeResponse


decodeResponse :: ByteString -> ServerResponse
decodeResponse = fromMaybe (error "Decoding Server Response Failed!") . Aeson.decodeStrict

