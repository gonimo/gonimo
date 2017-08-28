{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.Reflex.Dom.WebSocket.Internal where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Data.Text
import GHCJS.DOM.Types (liftJSM, JSM)
import qualified GHCJS.DOM.Types as JS
import Data.JSString.Text (textFromJSVal)
import qualified GHCJS.DOM.WebSocket as WS
import qualified GHCJS.DOM.CloseEvent as WS
import qualified GHCJS.DOM.MessageEvent as WS
import Control.Concurrent (forkIO)

import Data.Foldable (traverse_)
import GHCJS.DOM.EventM (on)
import Reflex.Dom.Core hiding (WebSocket, webSocket', webSocket)
import           Control.Monad.Reader                 (ask)
import qualified Language.Javascript.JSaddle as JS

import Gonimo.Client.Util

import Gonimo.Client.Reflex.Dom.WebSocket.Types

-- | Build up a websocket
-- - Connections get re-established on close.
-- - Messages from `configSend` will simply get dropped when socket is not ready.
webSocket :: forall t m. WebSocketM t m => Text -> Config t -> m (WebSocket t)
webSocket url config = mdo
    (jsOpenEvent, triggerJSOpen) <- newTriggerEvent
    (jsMessageEvent, triggerJSMessage) <- newTriggerEvent
    (jsErrorEvent, triggerJSError) <- newTriggerEvent
    (jsCloseEvent, triggerJSClose) <- newTriggerEvent

    let webSocket' = WebSocket { _open = jsOpenEvent
                               , _receive = jsMessageEvent
                               , _error = jsErrorEvent
                               , _close = close'
                               , _ws = ws'
                               , _jsClose = jsCloseEvent
                               , _triggerOpen = triggerJSOpen ()
                               , _triggerClose = triggerJSClose
                               , _triggerReceive = triggerJSMessage
                               , _triggerError = triggerJSError ()
                               }

    handleSendRequest webSocket'                                         $ config^.configSend
    close' <- handleCloseRequest webSocket' (config^.configCloseTimeout) $ config^.configClose

    ws' <- provideWebSocket webSocket' url $ fmap snd close'

    pure webSocket'

-- | Forwards user send request to the JS WebSocket object.
handleSendRequest :: forall t m. WebSocketM t m => WebSocket t -> Event t [Text] -> m ()
handleSendRequest webSocket' sendRequest
  = performEvent_ $ safeSend <$> attach currentWS sendRequest
  where
    currentWS = current $ webSocket'^.ws

    -- fromJSFunc () catches any exception and logs it to the console ...
    safeSend (ws', msgs) = traverse_ (fromJSFunc () . WS.sendString ws') msgs

-- | Handle user close requests & JS close events.
-- User close event triggers call to JS close.
-- Returns leftmost of delayed user event (delayed with `closeTimeout`) and JS close event.
handleCloseRequest
  :: forall t m. WebSocketM t m
  => WebSocket t
  -> Maybe Word -- ^ close timeout
  -> Event t CloseParams -- ^ Params to use for JS close request
  -> m (Event t (Bool, CloseParams))
handleCloseRequest webSocket' closeTimeout' closeRequest = do
    performEvent_ $ safeClose <$> attach currentWS closeRequest

    delayedRequest <- delayCloseRequest

    doForceClose <- hold False $ leftmost [ const True <$> closeRequest
                                          , const False <$> webSocket'^.jsClose
                                          ]
    let forceClose = fmap snd . ffilter fst $ attach doForceClose delayedRequest

    pure $ leftmost [ (False, ) <$> forceClose
                    , webSocket'^.jsClose
                    ]
  where
    currentWS = current $ webSocket'^.ws

    -- ws.close with exception handling on current WebSocket:
    safeClose (ws', CloseParams code reason) = fromJSFunc () $ WS.close ws' (Just code) (Just reason)
    delayCloseRequest = maybe (pure never) (flip delay closeRequest . fromIntegral) closeTimeout'

-- | Provide a websocket and create new one upon request,
-- cleaning up the old one. If the connection needs to be closed the provided code and reason are used.
-- TODO: Delay new socket creation for one second.
provideWebSocket :: forall t m. WebSocketM t m => WebSocket t -> Text -> Event t CloseParams -> m (Dynamic t JS.WebSocket)
provideWebSocket webSocket' url makeNew = mdo
    wsInit <- WS.newWebSocket url ([] :: [Text])

    cleanupInit <- liftJSM $ registerJSHandlers webSocket' wsInit

    newWS <- performEvent $ renew <$> attach (current connCleanup) makeNew

    connCleanup <- holdDyn (wsInit, cleanupInit) newWS
    pure $ fst <$> connCleanup
  where
    renew ((ws', cleanup'), ps) = liftJSM $ do
      state <- WS.getReadyState ws'
      unless (state == WS.CLOSING || state == WS.CLOSED)
        $ WS.close ws' (Just $ ps^.closeCode) (Just $ ps^.closeReason)
      cleanup'
      newWS <- WS.newWebSocket url ([] :: [Text])
      newCleanup <- registerJSHandlers webSocket' newWS
      pure (newWS, newCleanup)

-- | Registers handlers for events of the JS WebSocket.
-- Returns an action that can be triggerd for unregistering the handlers again.
registerJSHandlers :: WebSocket t -> WS.WebSocket -> JSM (JSM ())
registerJSHandlers webSocket' ws' = do
    state <- liftJSM $ WS.getReadyState ws'
    when (state == WS.OPEN) $ liftIO $ do
      putStrLn "!Websocket was already open - fire open event!"
      liftIO $ webSocket'^.triggerOpen -- Already open? -> Fire!
    releaseOnOpen <- on ws' WS.open $ do
      liftIO $ putStrLn "onOpen fired due to JS event"
      liftIO .void . forkIO $ webSocket'^.triggerOpen


    releaseOnClose <- on ws' WS.closeEvent $ do
      e <- ask
      wasClean <- WS.getWasClean e
      code <- WS.getCode e
      reason <- WS.getReason e
      liftIO $ putStrLn "WebSocket got closed! (JS event)"
      liftIO . void . forkIO $ (webSocket'^.triggerClose) (wasClean, CloseParams code reason)


    releaseOnMessage <- on ws' WS.message $ do
      e <- ask
      d <- WS.getData e
      str <- liftJSM $ JS.ghcjsPure . textFromJSVal $ d
      -- Fork off, so on message handler does not get blocked too long.
      liftIO . void . forkIO $ webSocket'^.triggerReceive $ str


    releaseOnError <- on ws' WS.error $ do
      liftIO . void . forkIO $ webSocket'^.triggerError

    pure $ do
      releaseOnOpen
      releaseOnClose
      releaseOnMessage
      releaseOnError
