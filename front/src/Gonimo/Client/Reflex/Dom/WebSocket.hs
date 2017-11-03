{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Gonimo.Client.Reflex.Dom.WebSocket ( Config(..)
                                          , WebSocket
                                          , HasConfig(..)
                                          , HasWebSocket(..)
                                          , CloseParams(..)
                                          , create
                                          , closeCode
                                          , closeReason
                                          ) where

import Gonimo.Client.Reflex.Dom.WebSocket.Internal
import Data.Text (Text)
import Control.Lens

-- | Build up a websocket
-- - Connections get re-established on `configOnClose`.
-- - Messages from `configOnSend` will simply get dropped when socket is not ready.
create :: forall t m . WebSocketM t m => Text -> Config t -> m (WebSocket t)
create url c = mdo
    let webSocket' = WebSocket {..}

    _events <- newTriggerEvents

    sendMessage webSocket'                               $ c^.configOnSend
    _onClose <- close webSocket' (c^.configCloseTimeout) $ c^.configOnClose

    _ws <- renew webSocket' url

    pure webSocket'
