{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex.Dom where

import Reflex.Dom
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

buttonAttr :: DomBuilder t m => Map Text Text -> m () -> m (Event t ())
buttonAttr attrs inner = do
  (e, _) <- elAttr' "button" attrs inner
  return $ domEvent Click e

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
  liftJSM $ JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ stream
