{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex.Dom where

import Control.Monad.IO.Class
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Data.Monoid
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Control.Lens
import GHCJS.DOM.Types (MediaStream, liftJSM, MonadJSM)
import qualified Language.Javascript.JSaddle                       as JS
import Data.Time.Clock
import Gonimo.Client.Prelude
import Gonimo.Client.Util
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.MediaStream             as MediaStream
import           GHCJS.DOM.MediaStreamTrack     (ended)
import Data.Maybe


renderVolumemeter :: forall m t. (HasWebView m, MonadWidget t m) => Event t Double -> m ()
renderVolumemeter volEvent = do
    elClass "div" "volumemeter" $ do
      volDyn <- holdDyn 0 $ (*1.4) <$> volEvent
      traverse_ (renderVolBarItem volDyn) [0.001, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
  where
    renderVolBarItem :: Dynamic t Double -> Double -> m ()
    renderVolBarItem currentVolume minVal = do
      let isActive = uniqDyn $ (\cv -> if cv > minVal then "volBarItemActive" else "") <$> currentVolume
      let highVolume = if minVal > 0.6 then "high-volume " else ""
      elDynClass "div" ( pure "volBarItem " <> pure highVolume <> isActive) $ blank

dismissibleOverlay :: forall m t. ( PerformEvent t m, MonadFix m, TriggerEvent t m, MonadHold t m
                                  , MonadIO (Performable m), MonadIO m, DomBuilder t m
                                  , PostBuild t m
                                  )
                      => Text -> NominalDiffTime -> m () -> m ()
dismissibleOverlay className dismissedAfter content = mdo
    dismissedEv <- delayed dismissedAfter
    dismissed <- holdDyn False $ const True <$> leftmost [dismissedEv, clicked]
    let shown = not <$> dismissed
    evClicked <-  dyn $ displayOverlay <$> shown
    clicked <- switchPromptly never evClicked
    pure ()
  where
    displayOverlay False = pure never
    displayOverlay True = makeClickable . elAttr' "div" ( "class" =: ("dismissible-overlay " <> className)
                                                        <> "role" =: "button") $ content

enterPressed :: Reflex t => Event t Int -> Event t ()
enterPressed = push (\key -> pure $ if key == 13
                                    then Just ()
                                    else Nothing
                    )

addBtnAttrs :: Text -> Map Text Text
addBtnAttrs className = "class" =: className <> "type" =: "button" <> "role" =: "button"

addBtnDynAttrs :: Reflex t => Dynamic t Text -> Dynamic t (Map Text Text)
addBtnDynAttrs className = fmap ("class" =:) className <> pure ("type" =: "button" <> "role" =: "button")

buttonAttr :: DomBuilder t m => Map Text Text -> m () -> m (Event t ())
buttonAttr attrs inner = makeClickable $ elAttr' "button" attrs inner

makeClickable :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ()) -> m (Event t ())
makeClickable item = do
  (e, _) <- item
  return $ domEvent Click e

myCheckBox :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Bool -> m () -> m (Event t Bool)
myCheckBox attrs checked inner = do
  let markedStr active = if active then "active " else ""
  (e, _) <- elAttr' "div" (attrs <> "role" =: "button") $ do
    inner
    elDynClass "div" (pure "switch " <> fmap markedStr checked) $ do
      elClass "div" "switch-out" blank
      elClass "div" "switch-in" blank
  let pressed = domEvent Mouseup e
  pure $ pushAlwaysCheap (\_ -> not <$> (sample $ current checked)) pressed

tabBar :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
          => Text -> Text -> Map k (Dynamic t Text -> m (Event t ())) -> m (Demux t (Maybe k))
tabBar ulClass activeClass tabItems = do
    let t0 = listToMaybe $ Map.keys tabItems
    rec currentTab :: Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass) $ do
          tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k s -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
          let eTabClicks :: Event t k = leftmost tabClicksList
          fmap demux $ holdDyn t0 $ fmap Just eTabClicks
    pure currentTab
  where
    headerBarLink :: (Dynamic t Text -> m (Event t ())) -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      clicked <- x $ fmap (\b -> if b then activeClass else "") isSelected
      return $ fmap (const k) clicked

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
customTabDisplay :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Dynamic t Text -> m (Event t ()), m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
customTabDisplay ulClass activeClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass) $ do
        tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k (s,_) -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
  el "div" $ do
    iforM_ tabItems $ \k (_, w) -> do
      let isSelected = demuxed currentTab $ Just k
          attrs = ffor isSelected $ \s -> if s then Map.empty else Map.singleton "style" "display:none;"
      elDynAttr "div" attrs w
    return ()
  where
    headerBarLink :: (Dynamic t Text -> m (Event t ())) -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      clicked <- x $ fmap (\b -> if b then activeClass else "") isSelected
      return $ fmap (const k) clicked

-- foreign import javascript unsafe "$1[\"srcObject\"] = $2;"
--         setSrcObject :: DOM.Element -> MediaStream -> IO ()
-- foreign import javascript unsafe "$1.play();"
--         playVideo :: DOM.Element -> IO ()

mediaVideo :: ( DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace
              , MonadJSM (Performable m), PerformEvent t m
              )
              => MediaStream -> Map Text Text -> m ()
mediaVideo stream attrs = do
  (videoTag, _) <- elAttr' "video" attrs blank
  let rawElement =  _element_raw videoTag
  -- liftIO $ setSrcObject rawElement stream
  -- liftIO $ playVideo rawElement
  liftJSM $ do
    JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ stream
    _ <- JS.toJSVal rawElement JS.# ("play" :: Text) $ ([] :: [JS.JSVal])
    when (Map.member "muted" attrs) $ do -- necessary in firefox
      _ <- JS.toJSVal rawElement JS.<# ("mute" :: Text) $ True
      pure ()
    registerTriggerFullScreen rawElement
    tracks <- catMaybes <$> MediaStream.getTracks stream
  -- Cleanup necessary because of DOM node leak in reflex, see: https://bugs.chromium.org/p/chromium/issues/detail?id=255456
    let registerCleanup track = do
          _ <- on track ended . liftJSM
               $ JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ JS.JSNull
          pure ()
    traverse_ registerCleanup tracks
    pure ()

-- mediaVideo :: ( DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace
--               , PostBuild t m, PerformEvent t m, MonadJSM (Performable m)
--               )
--               => MediaStream -> Map Text Text -> m ()
-- mediaVideo stream attrs = do
--   (videoTag, _) <- elAttr' "video" attrs blank
--   postBuild <- getPostBuild
--   let rawElement =  _element_raw videoTag
--   let
--     setURL :: MonadJSM m1 => m1 ()
--     setURL
--       = liftJSM $ do
--           JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ stream
--           _ <- JS.toJSVal rawElement JS.# ("play" :: Text) $ ([] :: [JS.JSVal])
--           pure ()
--   performEvent_ $ const setURL <$> postBuild


-- Make a dynamic attribute list with the given attribute toggled on dyn changes.
toggleAttr :: Reflex t => Text -> Dynamic t Bool -> Map Text Text -> Dynamic t (Map Text Text)
toggleAttr attr onOff staticAttrs =
  let
    attrDyn = (\on' -> if on' then Map.empty else attr =: "true") <$> onOff
  in
    pure staticAttrs <> attrDyn


addFocus :: MonadJSM m => InputElement EventResult GhcjsDomSpace t -> m ()
addFocus htmlEl = liftJSM $ do
  let rawElement = _inputElement_raw htmlEl
  _ <- JS.toJSVal rawElement JS.# ("focus" :: Text) $ ()
  _ <- JS.toJSVal rawElement JS.# ("select" :: Text) $ ()
  pure ()

addFocusPostBuild :: ( DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace
            , PostBuild t m, MonadJSM (Performable m) , PerformEvent t m
            )
            => InputElement EventResult (DomBuilderSpace m) t -> m ()
addFocusPostBuild htmlEl = do
  postBuild <- getPostBuild
  performEvent_ $ const (addFocus htmlEl) <$> postBuild -- Make sure it works always

delayed :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadIO m) => NominalDiffTime -> m (Event t ())
delayed dt = do
  (ev, trigger) <- newTriggerEvent
  liftIO $ trigger ()
  delay dt ev
