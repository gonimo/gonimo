{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.EditStringButton (editStringButton, editStringEl) where

import Reflex.Dom.Core
import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Control.Monad.Fix (MonadFix)
import Gonimo.Client.Reflex.Dom
import GHCJS.DOM.Types (MonadJSM)

type EditStringConstraint t m= (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM m, MonadJSM (Performable m), PerformEvent t m)

editStringButton :: forall t m. EditStringConstraint t m
                      => Map Text Text -> m () -> m () -> Dynamic t Text -> m (Event t Text)
editStringButton attrs inner = editStringEl (buttonAttr attrs inner)

-- Button like element for editing a string:
editStringEl :: forall t m. EditStringConstraint t m
                      => m (Event t ()) -> m () -> Dynamic t Text -> m (Event t Text)
editStringEl someButton editStringText val = mdo
  clicked <- someButton
  editStringDialog <- holdDyn (pure never) $ leftmost [ const (editStringBox editStringText val) <$> clicked
                                                      , const (pure never) <$> gotAnswer
                                                      ]
  gotAnswer <- switchPromptly never =<< dyn editStringDialog
  pure $ push (pure . id) gotAnswer


editStringBox :: forall t m. EditStringConstraint t m => m () -> Dynamic t Text -> m (Event t (Maybe Text))
editStringBox editStringText val = do
  elClass "div" "fullScreenOverlay" $
    elClass "div" "container" $ do
      cancelClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
      el "h1" editStringText
      el "br" blank
      el "br" blank

      el "h3" $ text "EDIT NAME"
      elClass "div" "welcome-form" $ do
        val' <- sample $ current val
        valEdit <-
          textInput $ def & textInputConfig_initialValue .~ val'
                          & textInputConfig_attributes .~ pure ( "class" =: "welcome-input" )
        addFocusPostBuild $ valEdit^.textInput_builderElement
        okClicked <- makeClickable . elAttr' "div" (addBtnAttrs "input-btn check") $ blank
        let confirmed = leftmost [ okClicked, keypress Enter valEdit ]
        let cancelled = leftmost [ cancelClicked, keypress Escape valEdit ]
        let editValue = current $ valEdit^.textInput_value
        pure $ leftmost [ const Nothing <$> cancelled, Just <$> tag editValue confirmed ]

