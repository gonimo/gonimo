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
import Data.Monoid

type EditStringConstraint t m= (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)

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
  elClass "div" "hCenteredOverlay fullScreen" $ do
    elClass "div" "vCenteredBox" $ do
      elClass "div" "panel panel-default" $ do
        elClass "div" "panel-heading" $ elClass "h3" "panel-title" $ editStringText
        elClass "div" "panel-body" $ do
          valEdit <- el "div" $ do
            val' <- sample $ current val
            let inputId = "editStringBox_TheOnlyOne"
            textInput $ def & textInputConfig_initialValue .~ val'
                                       & textInputConfig_attributes .~ pure ( "class" =: "form-control"
                                                                            <> "autofocus" =: "true"
                                                                            <> "id" =: inputId)
            -- el "script" $ text ("document.getElementById('" <> inputId <> "').focus();")
            -- pure v
          el "br" blank
          el "div" $ do
            okClicked <- buttonAttr ("class" =: "btn btn-success" <> "role" =: "button" <> "type" =: "button") $ text "Ok"
            cancelClicked <- buttonAttr ("class" =: "btn btn-danger" <> "role" =: "button" <> "type" =: "button") $ text "Cancel"
            let confirmed = leftmost [ okClicked, keypress Enter valEdit ]
            let cancelled = leftmost [ cancelClicked, keypress Escape valEdit ]
            let editValue = current $ valEdit^.textInput_value
            pure $ leftmost [ const Nothing <$> cancelled, Just <$> tag editValue confirmed ]

