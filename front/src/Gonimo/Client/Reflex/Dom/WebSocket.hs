module Gonimo.Client.Reflex.Dom.WebSocket where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.Class
import Gonimo.Client.Reflex.Dom.WebSocket.Foreign
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.Maybe (isJust)
import Data.Text
import GHCJS.DOM.Types (runJSM, askJSM, MonadJSM, liftJSM, JSM)
import qualified Language.Javascript.JSaddle.Monad as JS (catch)
import Data.JSString.Text (textFromJSVal)

import Gonimo.Client.Util


data CloseParams
  = CloseParams {
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                  _closeCode :: Word
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                , _closeReason :: Text
                }

data Config t
   = Config { _configSend :: Event t [Text]
            , _configClose :: Event t CloseParams
              -- | Timeout in seconds we wait for the JavaScript WebSocket connection
              -- to send it's close event, before we disconnect all event handlers and trigger
              -- '_webSocket_close' ourselves. Pass 'Nothing' if you want to
              -- wait indefinitely for the JS implementation.
            , _configCloseTimeout :: Maybe Word
            }

data WebSocket t
   = WebSocket { _open :: Event t ()
               , _receive :: Event t Text
               -- | error event does not carry any data and is always
               -- followed by termination of the connection
               -- for details see the close event
               , _error :: Event t ()
               , _close :: Event t ( Bool -- ^ wasClean
                                   , CloseParams
                                   )
               -- Internal stuff starts here:
               -- | Close event from JS WebSocket.
               , _jsClose :: Event t ( Bool
                                     , Closeparams
                                     )
               , _triggerOpen :: IO ()
               , _triggerClose :: (Bool, CloseParams) -> IO ()
               , _triggerReceive :: Text -> IO ()
               , _triggerError :: IO ()
               }


data Environment t
  = Environment { _config :: WebSocketConfig t
                , _webSocket :: WebSocket t
                }

makeLenses ''Config
makeLenses ''WebSocket
makeLenses ''Environment




-- | Build up a websocket
-- - Connections get re-established on close.
-- - Messages from `configSend` will simply get dropped when socket is not ready.
webSocket :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => Text -> Config t -> m (WebSocket t)
webSocket url config' = mdo
    (jsOpenEvent, triggerJSOpen) <- newTriggerEvent
    (jsMessageEvent, triggerJSMessage) <- newTriggerEvent
    (jsErrorEvent, triggerJSError) <- newTriggerEvent
    (jsCloseEvent, triggerJSClose) <- newTriggerEvent

    let webSocket' = WebSocket { _open = jsOpenEvent
                               , _receive = jsMessageEvent
                               , _error = jsErrorEvent
                               , _close = close'
                               , _jsClose = jsCloseEvent
                               , _triggerOpen = triggerJSOpen
                               , _triggerClose = triggerJSClose
                               , _triggerReceive = triggerJSMessage
                               , _triggerError = triggerJSError
                               }

    let env = Environtment { _config = config'
                           , _webSocket = webSocket'
                           }


    -- fromJS () catches any exception and logs it to the console ...
    performEvent_ $ fromJS () . uncurry JS.sendString <$> attach (current ws) (config'^.configSend)

    -- User exposed close event:
    close' <- handleCloseEvents env
    ws <- provideWebSocket . fmap snd $ close'

    pure webSocket'

-- | Handle user close requests & JS close events.
-- User close event triggers call to JS close.
-- Returns leftmost of delayed user event (delayed with `closeTimeout`) and JS close event.
handleCloseEvents ::  forall m t. _ m => Environment t -> JS.WebSocket -> m (Event t (Bool, CloseParams))
handleCloseEvents env ws = do
  let userClose = env^.config.configClose

  -- ws.close with exception handling on current WebSocket:
  performEvent_ $ fromJS () . uncurry JS.close <$> attach (current ws) userClose

  forceClose <- maybe (pure never) (flip delay userClose) $ env^.config.configCloseTimeout

  pure $ leftmost [ (False, ) <$> forceClose
                  , env^.webSocket.jsClose
                  ]

-- | Provide a websocket and create new one upon request,
-- cleaning up the old one. If the connection needs to be closed the provided code and reason are used.
provideWebSocket :: _ m => WebSocket t -> Event t CloseParams -> m (Dynamic t WS.WebSocket)
provideWebSocket webSocket' makeNew = mdo
    wsInit <- newWebSocket url []

    cleanupInit <- registerJSHandlers webSocket' wsInit

    let makeNew' = pushAlways (\closeParams -> do
                                  ws <- sample $ current connCleanup
                                  pure (ws, closeParams)
                              ) makeNew

    newWs <- performEvent $ renew <$> makeNew'

    connCleanup <- holdDyn (wsInit, cleanupInit) newWS
    pure $ fst <$> conn
  where
    renew ((ws, cleanup'), ps) = do
      state <- WS.getReadyState ws
      unless (state == WS.CLOSING || state == WS.CLOSED)
        $ WS.close ws (ps^.closeCode) (ps^.closeReason)
      cleanup'
      newWS <- newWebSocket url []
      newCleanup <- registerJSHandlers webSocket' newWS
      pure (newWs, newCleanup)

-- | Registers handlers for events of the JS WebSocket.
-- Returns an action that can be triggerd for unregistering the handlers again.
registerJSHandlers :: WebSocket t -> WS.WebSocket -> JSM (JSM ())
registerJSHandlers webSocket' ws = do
    state <- liftJSM $ WS.getReadyState ws
    when (state == WS.OPEN) $ liftIO $ do
      putStrLn "!Websocket was already open - fire open event!"
      onOpen -- Already open? -> Fire!
    releaseOnOpen <- on ws JS.open $ do
      liftIO $ putStrLn "onOpen fired due to JS event"
      liftJSM onOpen


    releaseOnClose <- on ws WS.closeEvent $ do
      e <- ask
      wasClean <- getWasClean e
      code <- getCode e
      reason <- getReason e
      liftIO $ "WebSocket got closed! (JS event)"
      liftIO $ (webSocket'^.triggerClose) (wasClean, CloseParams code reason)


    releaseOnMessage <- on ws WS.message $ do
      e <- ask
      d <- getData e
      str <- liftJSM $ ghcjsPure . textFromJSVal $ d
      liftIO $ webSocket'^.triggerReceive $ str


    releaseOnError <- on ws WS.error $ do
      liftIO $ webSocket'^.triggerError

    pure $ do
      releaseOnOpen
      releaseOnClose
      releaseOnMessage
      releaseOnError


instance Default CloseParams where
  def = CloseParams { _closeCode = 1000
                    , _closeReason = T.empty
                    }


instance Reflex t => Default (WebSocketconfig t a) where
  def = WebSocketconfig { _configSend = never
                        , _configClose = never
                        , _configTimeout = Nothing
                        }
