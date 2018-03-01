{-# LANGUAGE RecursiveDo #-}
module Gonimo.Client.Settings.UI where

import Reflex.Dom.Core

import Gonimo.Client.Prelude
import Gonimo.I18N
import Gonimo.Client.Reflex.Dom


langSelector :: forall model t m. GonimoM model t m => m (Event t Locale)
langSelector = elClass "div" "lang-select" $ mdo
    locDyn <- view locale
    let flagClass = pure "flag " <> fmap localeCssClass locDyn
    clicked <-
      makeClickable . elDynAttr' "div" (addBtnDynAttrs flagClass) $ blank
    pure $ pushAlways (const $ do
                          cLoc <- sample $ current locDyn
                          pure $ case cLoc of
                                   DE_DE -> EN_GB
                                   EN_GB -> DE_DE
                      ) clicked
  --   let openClose = pushAlways (\_ -> not <$> sample (current droppedDown)) clicked
  --   droppedDown <- holdDyn False $ leftmost [ openClose
  --                                           , const False <$> selectedName
  --                                           ]
  --   let
  --     droppedDownClass :: Dynamic t Text
  --     droppedDownClass = fmap (\opened -> if opened then "isDroppedDown " else "") droppedDown
  --   let
  --     dropDownClass :: Dynamic t Text
  --     dropDownClass = pure "dropDown-container " <> droppedDownClass

  --   selectedName <-
  --     elDynClass "div" dropDownClass $ renderLangSelectors [DE_DE, EN_GB]
  --   pure selectedName
  -- where

  --   renderLangSelectors :: [Locale] -> m (Event t Locale)
  --   renderLangSelectors langs =
  --     elClass "div" "" $
  --       leftmost <$> traverse renderLangSelector langs

  --   renderLangSelector :: Locale -> m (Event t Locale)
  --   renderLangSelector label = do
  --       clicked <-
  --         makeClickable
  --         . elAttr' "div" (addBtnAttrs $ "flag " <> localeCssClass label) $ blank
  --       pure $ const label <$> clicked

localeCssClass :: Locale -> Text
localeCssClass loc
  = case loc of
      DE_DE -> "flag-de"
      EN_GB -> "flag-gb"
