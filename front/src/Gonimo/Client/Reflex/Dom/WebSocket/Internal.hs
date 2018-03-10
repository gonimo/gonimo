{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Gonimo.Client.Reflex.Dom.WebSocket.Internal where

import Prelude -- hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class
import Control.Lens
import Control.Monad -- hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Data.Text
import qualified GHCJS.DOM.Types as JS
import Data.JSString.Text (textFromJSVal)
import qualified GHCJS.DOM.WebSocket as WS
import qualified GHCJS.DOM.CloseEvent as WS
import qualified GHCJS.DOM.MessageEvent as WS
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Data.Default
import Control.Monad.Fix
import GHCJS.DOM.Types (MonadJSM, liftJSM, JSM)
import qualified Data.Text as T

import Data.Foldable (traverse_)
import GHCJS.DOM.EventM (on)
import Reflex.Dom.Core hiding (WebSocket, webSocket', webSocket)
import           Control.Monad.Reader                 (ask)
import qualified Language.Javascript.JSaddle as JS hiding (MonadJSM)

import Gonimo.Client.Util

class HasConfig a where
  config :: Lens' (a t) (Config t)

  -- | Send messages over the WebSocket
  configOnSend :: Lens' (a t) (Event t [Text])
  configOnSend = config . go
    where
      go :: Lens' (Config t) (Event t [Text])
      go f cfg' = (\configOnSend' -> cfg' { _configOnSend = configOnSend' }) <$> f (_configOnSend  cfg')

  -- | Close the Config. (It will be re-opened!)
  configOnClose :: Lens' (a t) (Event t CloseParams)
  configOnClose = config . go
    where
      go :: Lens' (Config t) (Event t CloseParams)
      go f cfg' = (\configOnClose' -> cfg' { _configOnClose = configOnClose' }) <$> f (_configOnClose  cfg')

  -- | Timeout after close, until close is forced.
  --   Nothing: No timeout, on configOnClose we will simply tell the WebSocket to
  --   close and wait for it to finish.
  --   Just x: We tell the websocket to close, but after x seconds we will
  --   simply abandon it and create a new one. Disconnecting and ignoring the old one.
  configCloseTimeout :: Lens' (a t) (Maybe Word)
  configCloseTimeout = config . go
    where
      go :: Lens' (Config t) (Maybe Word)
      go f cfg' = (\configCloseTimeout' -> cfg' { _configCloseTimeout = configCloseTimeout' }) <$> f (_configCloseTimeout  cfg')


class HasWebSocket a where
  webSocket :: Lens' (a t) (WebSocket t)

  -- | Socket opened event.
  onOpen :: Lens' (a t) (Event t ())
  onOpen = webSocket . go
    where
      go :: Lens' (WebSocket t) (Event t ())
      go f ws' = (\onOpen' -> ws' { _events = (_events ws') { _onOpen = onOpen' }}) <$> f ((_onOpen . _events) ws')

  -- | Some error occurred. Always followed by an `onClose` event.
  onError :: Lens' (a t) (Event t ())
  onError = webSocket . go
    where
      go :: Lens' (WebSocket t) (Event t ())
      go f ws' = (\onError' -> ws' { _events = (_events ws') { _onError = onError' }}) <$> f ((_onError . _events) ws')

  -- | A message was received.
  onReceive :: Lens' (a t) (Event t Text)
  onReceive = webSocket . go
    where
      go :: Lens' (WebSocket t) (Event t Text)
      go f ws' = (\onReceive' -> ws' { _events = (_events ws') { _onReceive = onReceive' }}) <$> f ((_onReceive . _events) ws')

  -- | WebSocket got closed for some reason. It will be re-opened automatically.
  --   This event gets also triggered on close timeout. (Rational: We consider
  --   it closed and will open a new one.)
  onClose :: Lens' (a t) (Event t (Bool, CloseParams))
  onClose = webSocket . go
    where
      go :: Lens' (WebSocket t) (Event t (Bool, CloseParams))
      go f ws' = (\onClose' -> ws' { _onClose = onClose' }) <$> f (_onClose ws')


data Config t
   = Config { _configOnSend :: Event t [Text]
            , _configOnClose :: Event t CloseParams
            , _configCloseTimeout :: Maybe Word
            }

instance HasConfig Config where
  config = id

data WebSocket t
   = WebSocket { _events :: Events t
               , _ws :: Dynamic t JS.WebSocket -- ^ JavaScript WebSocket object
               -- | Close event from JS WebSocket, or forced close. (We abandoned the old websocket)
               , _onClose :: Event t ( Bool
                                     , CloseParams
                                     )
               }

instance HasWebSocket WebSocket where
  webSocket = id


data CloseParams
  = CloseParams {
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                  _closeCode :: Word
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                , _closeReason :: Text
                }


data Events t
  = Events { _onOpen :: Event t ()
           , _onReceive :: Event t Text
           -- | onError event does not carry any data and is always
           -- followed by termination of the connection
           -- for details see the close event
           , _onError :: Event t ()
             -- | (wasClean, CloseParams)
           , _jsOnClose :: Event t ( Bool
                                   , CloseParams
                                   )
           , _triggerOnOpen :: IO ()
           , _triggerJsOnClose :: (Bool, CloseParams) -> IO ()
           , _triggerOnReceive :: Text -> IO ()
           , _triggerOnError :: IO ()
           }

type WebSocketM t m = (MonadJSM m, MonadJSM (Performable m), HasJSContext m,
                       PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadFix m, MonadHold t m,
                       MonadIO (Performable m))


instance Default CloseParams where
  def = CloseParams { _closeCode = 1000
                    , _closeReason = T.empty
                    }


instance Reflex t => Default (Config t) where
  def = Config { _configOnSend = never
               , _configOnClose = never
               , _configCloseTimeout = Nothing
               }

newTriggerEvents :: forall t m. WebSocketM t m => m (Events t)
newTriggerEvents = do
    (_onOpen, _triggerOnOpen') <- newTriggerEvent
    let _triggerOnOpen = _triggerOnOpen' ()

    (_onReceive, _triggerOnReceive) <- newTriggerEvent

    (_onError, _triggerOnError') <- newTriggerEvent
    let _triggerOnError = _triggerOnError' ()

    (_jsOnClose, _triggerJsOnClose) <- newTriggerEvent

    pure Events{..}

-- | Forwards user send request to the JS WebSocket object.
sendMessage :: forall t m. WebSocketM t m => WebSocket t -> Event t [Text] -> m ()
sendMessage webSocket' onSend
  = performEvent_ $ safeSend <$> attach currentWS onSend
  where
    currentWS = current $ webSocket'^.ws

    -- fromJSFunc () catches any exception and logs it to the console ...
    safeSend (ws', msgs) = traverse_ (fromJSFunc () . WS.sendString ws') msgs

-- | Takes care of closing the WebSocket connection
-- `configOnClose` event triggers call to JS close.
-- Returns leftmost of delayed user event (delayed with `closeTimeout`) and JS close event.
close
  :: forall t m. WebSocketM t m
  => WebSocket t
  -> Maybe Word -- ^ close timeout
  -> Event t CloseParams -- ^ Params to use for JS close request
  -> m (Event t (Bool, CloseParams))
close webSocket' closeTimeout' onConfigClose = do
    performEvent_ $ uncurry safeClose <$> attach currentWS onConfigClose

    delayedRequest <- delayCloseRequest

    doForceClose <- hold False $ leftmost [ True  <$ onConfigClose
                                          , False <$ webSocket'^.jsOnClose
                                          ]
    let onForceClose = fmap snd . ffilter fst $ attach doForceClose delayedRequest

    pure $ leftmost [ (False, ) <$> onForceClose
                    , webSocket'^.jsOnClose
                    ]
  where
    currentWS = current $ webSocket'^.ws
    delayCloseRequest = maybe (pure never) (flip delay onConfigClose . fromIntegral) closeTimeout'

-- | Provide a websocket and create new one upon request,
-- cleaning up the old one. If the connection needs to be closed the provided
-- code and reason are used.
-- This code is actually quite a bit tricky. Beware of race conditions:
-- It can happen that a JS close event and a force close are handled both, this just results in a needless connection
-- re-establishment.
-- Don't simply block close events with gate or something until a new websocket
-- arrives, because an early close event of the new socket would be ignored,
-- resulting in a connection that never gets re-established!
renew :: forall t m. WebSocketM t m => WebSocket t -> Text -> m (Dynamic t JS.WebSocket)
renew webSocket' url = mdo
    let makeNew = fmap snd $ webSocket'^.onClose
    (wsInit, cleanupAction) <- initialize

    ws' <- holdDyn wsInit newWS

    renewInProgress <- hold False $ leftmost [ True  <$ makeNew
                                             , False <$ newWS
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
      = performEvent $ doRenew cleanupAction <$> attach (current ws') makeNew'

    doRenew :: JS.MonadJSM m1 => MVar (JSM ()) -> (JS.WebSocket, CloseParams) -> m1 JS.WebSocket
    doRenew cleanupAction (ws', ps) = liftJSM $ do
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
      webSocket'^.triggerOnOpen -- Already onOpen? -> Fire!

    when (state == WS.CLOSED) . liftIO $ do
      putStrLn "!Websocket was already closed - fire close event!"
      webSocket'^.triggerJsOnClose $ (False, CloseParams 4000 "Real close event got lost, faking it.")

    releaseOnOpen <- on ws' WS.open $ do
      liftIO $ putStrLn "onOpen fired due to JS event"
      liftIO $ webSocket'^.triggerOnOpen


    releaseOnClose <- on ws' WS.closeEvent $ do
      e <- ask
      wasClean <- WS.getWasClean e
      code <- WS.getCode e
      reason <- WS.getReason e
      liftIO $ putStrLn "WebSocket got closed! (JS event)"
      liftIO $ (webSocket'^.triggerJsOnClose) (wasClean, CloseParams code reason)


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
        liftIO $ webSocket'^.triggerOnReceive $ str

    releaseOnError <- on ws' WS.error $
      liftIO $ webSocket'^.triggerOnError

    pure $ do
      releaseOnOpen
      releaseOnClose
      releaseOnMessage
      releaseOnError

-- CloseParams lenses:

closeCode :: Lens' CloseParams Word
closeCode f cps' = (\closeCode' -> cps' { _closeCode = closeCode' }) <$> f (_closeCode cps')

closeReason :: Lens' CloseParams Text
closeReason f cps' = (\closeReason' -> cps' { _closeReason = closeReason' }) <$> f (_closeReason cps')



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

-- Lenses for internal use:

jsOnClose :: Lens' (WebSocket t) (Event t (Bool, CloseParams))
jsOnClose f ws' = (\jsOnClose' -> ws' { _events = (_events ws') { _jsOnClose = jsOnClose' }}) <$> f ((_jsOnClose . _events) ws')

triggerOnOpen :: Lens' (WebSocket t) (IO ())
triggerOnOpen f ws' = (\triggerOnOpen' -> ws' { _events = (_events ws') { _triggerOnOpen = triggerOnOpen' }}) <$> f ((_triggerOnOpen . _events) ws')

triggerJsOnClose :: Lens' (WebSocket t) ((Bool, CloseParams) -> IO ())
triggerJsOnClose f ws' = (\triggerJsOnClose' -> ws' { _events = (_events ws') { _triggerJsOnClose = triggerJsOnClose' }}) <$> f ((_triggerJsOnClose . _events) ws')

triggerOnReceive :: Lens' (WebSocket t) (Text -> IO ())
triggerOnReceive f ws' = (\triggerOnReceive' -> ws' { _events = (_events ws') { _triggerOnReceive = triggerOnReceive' }}) <$> f ((_triggerOnReceive . _events) ws')

triggerOnError :: Lens' (WebSocket t) (IO ())
triggerOnError f ws' = (\triggerOnError' -> ws' { _events = (_events ws') { _triggerOnError = triggerOnError' }}) <$> f ((_triggerOnError . _events) ws')

-- | JavaScript WebSocket implementation.
ws :: Lens' (WebSocket t) (Dynamic t WS.WebSocket)
ws f ws' = (\_ws' -> ws' { _ws = _ws' }) <$> f (_ws ws')
