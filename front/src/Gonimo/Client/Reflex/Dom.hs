{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex.Dom where

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

enterPressed :: Reflex t => Event t Int -> Event t ()
enterPressed = push (\key -> pure $ if key == 13
                                    then Just ()
                                    else Nothing
                    )
addBtnAttrs :: Text -> Map Text Text
addBtnAttrs className = "class" =: className <> "type" =: "button" <> "role" =: "button"

buttonAttr :: DomBuilder t m => Map Text Text -> m () -> m (Event t ())
buttonAttr attrs inner = makeClickable $ elAttr' "button" attrs inner

makeClickable :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ()) -> m (Event t ())
makeClickable item = do
  (e, _) <- item
  return $ domEvent Click e

myCheckBox :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Bool -> m () -> m (Event t Bool)
myCheckBox attrs checked inner = do
  let markedStr active = if active then "active " else ""
  let markedAttr = ("class" =:) . markedStr <$> checked
  let allAttrs = zipDynWith (Map.unionWith (<>)) markedAttr (pure attrs)
  (e, _) <- elDynAttr' "button" allAttrs inner
  let clicked = domEvent Click e
  pure $ push (\() -> Just . not <$> (sample $ current checked)) clicked

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

mediaVideo :: (DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
              => MediaStream -> Map Text Text -> m ()
mediaVideo stream attrs = do
  (videoTag, _) <- elAttr' "video" attrs blank
  let rawElement =  _element_raw videoTag
  liftJSM $ do
    JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ stream
    _ <- JS.toJSVal rawElement JS.# ("play" :: Text) $ ([] :: [JS.JSVal])
    pure ()

-- Make a dynamic attribute list with the given attribute toggled on dyn changes.
toggleAttr :: Reflex t => Text -> Dynamic t Bool -> Map Text Text -> Dynamic t (Map Text Text)
toggleAttr attr onOff staticAttrs =
  let
    attrDyn = (\on -> if on then Map.empty else attr =: "true") <$> onOff
  in
    pure staticAttrs <> attrDyn


addFocus :: ( DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace
            , PostBuild t m, MonadJSM (Performable m) , PerformEvent t m
            )
            => InputElement EventResult (DomBuilderSpace m) t -> m ()
addFocus htmlEl = do
  postBuild <- getPostBuild
  let rawElement = _inputElement_raw htmlEl
  let addFocus' = liftJSM $ do
        _ <- JS.toJSVal rawElement JS.# ("focus" :: Text) $ ()
        _ <- JS.toJSVal rawElement JS.# ("select" :: Text) $ ()
        pure ()
  performEvent_ $ const addFocus' <$> postBuild
