{- Stolen and modified from reflex-dom, thanks!

Copyright (c) 2015, Obsidian Systems LLC
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.Client.Reflex.Dom.WebSocket.Foreign
  ( module Gonimo.Client.Reflex.Dom.WebSocket.Foreign
  , JSVal
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding
import Gonimo.Client.Reflex.Dom.WebSocket.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import GHCJS.DOM.CloseEvent
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.Types hiding (Text)
import qualified GHCJS.DOM.WebSocket as GD
import GHCJS.Foreign (JSType(..), jsTypeOf)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)

data JSWebSocket = JSWebSocket { unWebSocket :: WebSocket
                               , releaseHandlers :: JSM ()
                               }

class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> JSM ()

-- Use binary websocket communication for ByteString
-- Note: Binary websockets may not work correctly in IE 11 and below
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws _) bs = do
    ab <- bsToArrayBuffer bs
    GD.send ws $ Just ab

-- Use plaintext websocket communication for Text, and String
instance IsWebSocketMessage Text where
  webSocketSend (JSWebSocket ws _) = GD.sendString ws . T.unpack

closeWebSocket :: JSWebSocket -> Word -> Text -> JSM ()
closeWebSocket (JSWebSocket ws _) = GD.close ws

-- Close but before disconnect all event handlers - so user can call onClose handlers himself
-- and be done with this connection immediately!
closeWebSocketBrutally :: JSWebSocket -> Word -> Text -> JSM ()
closeWebSocketBrutally (JSWebSocket ws releaseAll) code msg = do
  releaseAll
  GD.close ws code msg

webSocketGetReadyState :: JSWebSocket -> JSM Word
webSocketGetReadyState (JSWebSocket ws _) = GD.getReadyState ws

newWebSocket
  :: a
  -> Text -- url
  -> (Either ByteString JSVal -> JSM ()) -- onmessage
  -> JSM () -- onopen
  -> JSM () -- onerror
  -> ((Bool, Word, Text) -> JSM ()) -- onclose
  -> JSM JSWebSocket
newWebSocket _ url onMessage onOpen onError onClose = do
  ws <- GD.newWebSocket url (Just [] :: Maybe [Text])
  GD.setBinaryType ws "arraybuffer"
  releaseClose <- on ws GD.open $ liftJSM onOpen
  releaseOnError <- on ws GD.error $ liftJSM onError
  releaseOnClose <- on ws GD.closeEvent $ do
    e <- ask
    wasClean <- getWasClean e
    code <- getCode e
    reason <- getReason e
    liftJSM $ onClose (wasClean, code, reason)
  releaseOnMessage <- on ws GD.message $ do
    e <- ask
    d <- getData e
    liftJSM $ ghcjsPure (jsTypeOf d) >>= \case
      String -> onMessage $ Right d
      _ -> do
        ab <- mutableArrayBufferFromJSVal d
        bsFromMutableArrayBuffer ab >>= onMessage . Left
  return $ JSWebSocket ws $ do
    releaseClose
    releaseOnError
    releaseOnClose
    releaseOnMessage

onBSMessage :: Either ByteString JSVal -> JSM ByteString
onBSMessage = either return (\v -> encodeUtf8 <$> fromJSValUnchecked v)
