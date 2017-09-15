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
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)

import Data.Foldable (traverse_)
import GHCJS.DOM.EventM (on)
import Reflex.Dom.Core hiding (WebSocket, webSocket', webSocket)
import           Control.Monad.Reader                 (ask)
import qualified Language.Javascript.JSaddle as JS hiding (MonadJSM)

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
    performEvent_ $ uncurry safeClose <$> attach currentWS closeRequest

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

    delayCloseRequest = maybe (pure never) (flip delay closeRequest . fromIntegral) closeTimeout'

-- | Provide a websocket and create new one upon request,
-- cleaning up the old one. If the connection needs to be closed the provided
-- code and reason are used.
-- This code is actually quite a bit tricky. Beware of race conditions:
-- It can happen that a JS close event and a force close are handled both, this just results in a needless connection
-- re-establishment.
-- Don't block close events with gate or something until a new websocket
-- arrives, because an early close event of the new socket would be ignored,
-- resulting in a connection that never gets re-established!
provideWebSocket :: forall t m. WebSocketM t m => WebSocket t -> Text -> Event t CloseParams -> m (Dynamic t JS.WebSocket)
provideWebSocket webSocket' url makeNew = mdo
    (wsInit, cleanupAction) <- initialize

    ws' <- holdDyn wsInit newWS

    renewInProgress <- hold False $ leftmost [ const True <$> makeNew
                                             , const False <$> newWS
                                             ]

    let
      -- Avoid race condition, only allow one makeNew event to be processed at a time:
      makeNewGated :: Event t CloseParams
      makeNewGated = gate (not <$> renewInProgress) makeNew

    -- Avoid hoging CPU, wait for 1 second inbetween attempts:
    newWS <- performRenew cleanupAction ws' =<< delay 1 makeNewGated

    performSetupHandlers cleanupAction newWS

    pure ws'
  where
    initialize = do
      -- Needed for some sanity: With an MVar we can sync the asynchronous JS
      -- world with our FRP state and get consistent behaviour: JS events will
      -- target the right WebSocket. Otherwise an early `close` event would
      -- arrive before our ws' dynamic is updated and would get lost due to our
      -- gating above and without it would target the wrong websocket and would
      -- effectively get lost too.
      cleanupAction <- liftIO newEmptyMVar
      wsInit <- safeNewWebSocket url ([] :: [Text])
      liftJSM $ setupHandlers cleanupAction wsInit
      pure (wsInit, cleanupAction)

    performRenew :: MVar (JSM ()) -> Dynamic t JS.WebSocket
                 -> Event t CloseParams
                 -> m (Event t JS.WebSocket)
    performRenew cleanupAction ws' makeNew'
      = performEvent $ renew cleanupAction <$> attach (current ws') makeNew'

    renew :: JS.MonadJSM m1 => MVar (JSM ()) -> (JS.WebSocket, CloseParams) -> m1 JS.WebSocket
    renew cleanupAction (ws', ps) = liftJSM $ do
      runCleanup cleanupAction
      state <- WS.getReadyState ws'
      unless (state == WS.CLOSING || state == WS.CLOSED)
        $ safeClose ws' ps
      newWS <- safeNewWebSocket url ([] :: [Text])
      pure newWS

    performSetupHandlers cleanupAction
      = performEvent_ . fmap (liftJSM . setupHandlers cleanupAction)

    setupHandlers cleanupAction
      = liftIO . putMVar cleanupAction <=< registerJSHandlers webSocket'

    runCleanup = join . liftIO . takeMVar

-- | Registers handlers for events of the JS WebSocket.
-- Returns an action that can be triggerd for unregistering the handlers again.
registerJSHandlers :: WebSocket t -> WS.WebSocket -> JSM (JSM ())
registerJSHandlers webSocket' ws' = do
    state <- liftJSM $ WS.getReadyState ws'
    when (state == WS.OPEN) . liftIO $ do
      putStrLn "!Websocket was already open - fire open event!"
      webSocket'^.triggerOpen -- Already open? -> Fire!

    when (state == WS.CLOSED) . liftIO $ do
      putStrLn "!Websocket was already closed - fire close event!"
      webSocket'^.triggerClose $ (False, CloseParams 4000 "Real close event got lost, faking it.")

    releaseOnOpen <- on ws' WS.open $ do
      liftIO $ putStrLn "onOpen fired due to JS event"
      liftIO $ webSocket'^.triggerOpen


    releaseOnClose <- on ws' WS.closeEvent $ do
      e <- ask
      wasClean <- WS.getWasClean e
      code <- WS.getCode e
      reason <- WS.getReason e
      liftIO $ putStrLn "WebSocket got closed! (JS event)"
      liftIO $ (webSocket'^.triggerClose) (wasClean, CloseParams code reason)


    releaseOnMessage <- on ws' WS.message $ do
      e <- ask
      d <- WS.getData e

      -- TODO: Probably not really necessary, was just a try in the search of a bug:
      dIsNull <- liftJSM . JS.ghcjsPure . JS.isNull $ d
      dIsUndefined <- liftJSM . JS.ghcjsPure . JS.isUndefined $ d
      when (dIsNull || dIsUndefined) $ liftIO $ putStrLn "WebSocket message was undefined/null!"
      unless (dIsNull || dIsUndefined) $ do
        str <- liftJSM $ JS.ghcjsPure . textFromJSVal $ d
        -- Fork off, so on message handler does not get blocked too long.
        liftIO $ webSocket'^.triggerReceive $ str

    releaseOnError <- on ws' WS.error $ do
      liftIO $ webSocket'^.triggerError

    pure $ do
      releaseOnOpen
      releaseOnClose
      releaseOnMessage
      releaseOnError

-- | Internal helper function: Call close and catch all exceptions..
safeClose :: JS.MonadJSM m => WS.WebSocket -> CloseParams -> m ()
safeClose ws' (CloseParams code reason) = fromJSFunc () $ WS.close ws' (Just code) (Just reason)

safeNewWebSocket :: JS.MonadJSM m => Text -> [Text] -> m WS.WebSocket
safeNewWebSocket url protocols = safeNewWebSocket'
  where
    safeNewWebSocket' = do
      mWS <- fromJSFunc Nothing $ Just <$> WS.newWebSocket url protocols
      case mWS of
        Nothing -> do
          liftIO $ threadDelay 1000000 -- sleep for one second & try again ..
          safeNewWebSocket'
        Just ws' -> pure ws'
