{-- Stolen from reflex-dom-contrib for now, as it fails to build with the current reflex-platform.
 -- Adapted to use network-uri instead of uri-bytestring in order to get rid of TemplateHaskell. (Cross compiling you know ...)
--}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.Contrib.Router (
  -- == High-level routers
    route
  , route'

  -- = Low-level URL bar access
  , getLoc
  , getURI
  , getUrlText
  , URI

  -- = History movement
  , goForward
  , goBack
  , getHistoryLength
  , getHistoryPosition
  ) where

------------------------------------------------------------------------------
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHCJS.DOM                     (currentWindow)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTarget         (dispatchEvent_)
import           GHCJS.DOM.History             (History, back, forward,
                                                pushState)
import qualified GHCJS.DOM.History             as History
import           GHCJS.DOM.Location            (getHref)
import           GHCJS.DOM.PopStateEvent
import           GHCJS.DOM.Types               (Location (..))
import           GHCJS.DOM.Types               (MonadJSM,
                                                SerializedScriptValue (..))
import           GHCJS.DOM.Window              (getHistory, getLocation)
import qualified Network.URI                   as U
import           Reflex.Dom.Core               hiding (EventName, Window)
#if MIN_VERSION_ghcjs_dom(0,8,0)
import           GHCJS.DOM.WindowEventHandlers (popState)
#else
import           GHCJS.DOM.Window              (popState)
#endif
import           Control.Monad.IO.Class        (liftIO)
import           Data.IORef
import           Data.Maybe
import           GHCJS.Marshal.Pure            (pFromJSVal)
import           Language.Javascript.JSaddle   (JSM, Object (..),
                                                fromJSVal, liftJSM)
import qualified Language.Javascript.JSaddle   as JS
------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic
--   routing of a widget
--   These sources of URL-bar change will be reflected in the output URI
--     - Input events to 'route'
--     - Browser Forward/Back button clicks
--     - forward/back javascript calls (or 'goForward'/'goBack') Haskell calls
--     - Any URL changes followed by a popState event
--   But external calls to pushState that don't manually fire a popState
--   won't be detected
route
  :: (HasJSContext m, MonadWidget t m)
  => Event t T.Text
  -> m (Dynamic t URI)
route pushTo = do
  loc0    <- getURI
  -- history.length only tells you the complete history length, not the current position,
  -- therefore we store an index in the history state.
  historyIndexRef <- liftIO $ newIORef (1 :: Double)

  _ <- performEvent $ ffor pushTo $ \t -> do
    let newState =
#if MIN_VERSION_ghcjs_dom(0,8,0)
          Just t
#else
          t
#endif
    index <- liftIO $ readIORef historyIndexRef
    liftIO $ modifyIORef historyIndexRef (+1)
    withHistory $ \h -> pushState h index ("" :: T.Text) (newState :: Maybe T.Text)
    liftJSM dispatchEvent'

  locUpdates <- getPopState
  holdDyn loc0 locUpdates

route'
  :: forall t m a b. MonadWidget t m
  => (URI -> a -> URI)
  -> (URI -> b)
  -> Event t a
  -> m (Dynamic t b)
route' encode decode routeUpdate = do
  rec rUri <- route (T.pack . show <$> urlUpdates)
      let urlUpdates = attachWith encode (current rUri) routeUpdate
  return $ decode <$> rUri


-------------------------------------------------------------------------------
-- | Route a single page app according to the part of the path after
--   pathBase
-- partialPathRoute
--   :: forall t m. MonadWidget t m
--   => T.Text  -- ^ The path segments not related to SPA routing
--              --   (leading '/' will be added automaticaly)
--   -> Event t T.Text -- ^ Updates to the path segments used for routing
--                     --   These values will be appended to the base path
--   -> m (Dynamic t [T.Text]) -- ^ Path segments used for routing
-- partialPathRoute pathBase pathUpdates = do
--   route' (flip updateUrl) parseParts pathUpdates
--   where

--     toPath :: T.Text -> BS.ByteString
--     toPath dynpath = T.encodeUtf8 $
--       "/" <> cleanT pathBase <>
--       "/" <> cleanT dynpath

--     updateUrl :: T.Text -> URI -> URI
--     updateUrl updateParts u = u & U.pathL .~ toPath updateParts

--     parseParts :: URI -> [T.Text]
--     parseParts u =
--       maybe (error $ pfxErr u pathBase)
--             (T.splitOn "/" . T.decodeUtf8 . cleanB) .
--       BS.stripPrefix (T.encodeUtf8 $ cleanT pathBase) $
--       cleanB (u ^. U.pathL)

--     cleanT = T.dropWhile (=='/')
--     cleanB = BS.dropWhile (== '/')


-------------------------------------------------------------------------------


getPopState :: (MonadWidget t m) => m (Event t URI)
getPopState = do
  Just window <- currentWindow
  wrapDomEventMaybe window (`on` popState) $ do
#if MIN_VERSION_ghcjs_dom(0,8,0)
    loc
#else
    Just loc
#endif
      <- getLocation window
    locStr <- getHref loc
    return $ U.parseURI locStr


-------------------------------------------------------------------------------
goForward :: (HasJSContext m, MonadJSM m) => m ()
goForward = withHistory forward


-------------------------------------------------------------------------------
goBack :: (HasJSContext m, MonadJSM m) => m ()
goBack = withHistory back


-------------------------------------------------------------------------------

getHistoryLength :: (HasJSContext m, MonadJSM m) => m Word
getHistoryLength = withHistory History.getLength

-------------------------------------------------------------------------------

getHistoryPosition :: (HasJSContext m, MonadJSM m) => m Double
getHistoryPosition = fmap (fromMaybe 0) . liftJSM . fromJSVal . unSerializedScriptValue
                     =<< withHistory History.getState

-------------------------------------------------------------------------------

withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
withHistory act = do
  Just w <- currentWindow
#if MIN_VERSION_ghcjs_dom(0,8,0)
  h
#else
  Just h
#endif
    <- getHistory w
  act h


-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: (HasJSContext m, MonadJSM m) => m Location
getLoc = do
  Just win <- currentWindow
#if MIN_VERSION_ghcjs_dom(0,8,0)
  loc
#else
  Just loc
#endif
    <- getLocation win
  return loc


-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText :: (HasJSContext m, MonadJSM m) => m Text
getUrlText = getLoc >>= getHref


-------------------------------------------------------------------------------
type URI = U.URI


-------------------------------------------------------------------------------
getURI :: (HasJSContext m, MonadJSM m) => m URI
getURI = do
  l <- getUrlText
  return
    $ fromMaybe (error "No parse of window location")
    . U.parseURI . T.unpack $ l


dispatchEvent' :: JSM ()
dispatchEvent' = do
  Just window <- currentWindow
  obj@(Object o) <- JS.create
  JS.objSetPropertyByName obj ("cancelable" :: Text) True
  JS.objSetPropertyByName obj ("bubbles" :: Text) True
  JS.objSetPropertyByName obj ("view" :: Text) window
  event <- newPopStateEvent ("popstate" :: Text) $ Just $ pFromJSVal o
  dispatchEvent_ window event
