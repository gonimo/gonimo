{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.EditStringButton ( editStringButton
                                      , editStringEl
                                      , editDeviceName
                                      , editFamilyName
                                      ) where

import           Control.Lens
import           Data.Map                            (Map)
import           Data.Text                           (Text)
import           Reflex.Dom.Core

import           Gonimo.Client.EditStringButton.I18N
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom

editFamilyName :: forall model t m. GonimoM model t m
                      => m (Event t ()) -> Dynamic t Text -> m (Event t Text)
editFamilyName someButton = editStringEl someButton (trText Change_family_name_to)

editDeviceName :: forall model t m. GonimoM model t m
                      => m (Event t ()) -> Dynamic t Text -> m (Event t Text)
editDeviceName someButton = editStringEl someButton (trText Change_device_name_to)

editStringButton :: forall model t m. GonimoM model t m
                      => Map Text Text -> m () -> m () -> Dynamic t Text -> m (Event t Text)
editStringButton attrs = editStringEl . buttonAttr attrs

-- Button like element for editing a string:
editStringEl :: forall model t m. GonimoM model t m
                      => m (Event t ()) -> m () -> Dynamic t Text -> m (Event t Text)
editStringEl someButton editStringText val = mdo
  clicked <- someButton
  editStringDialog <- holdDyn (pure never) $
                        leftmost [ editStringBox editStringText val <$ clicked
                                 , pure never <$ gotAnswer
                                 ]
  gotAnswer <- switchHoldPromptly never =<< dyn editStringDialog
  pure $ push pure gotAnswer


editStringBox :: forall model t m. GonimoM model t m
              => m () -> Dynamic t Text -> m (Event t (Maybe Text))
editStringBox editStringText val = do
  elClass "div" "fullScreenOverlay" $
    elClass "div" "container" $ do
      cancelClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
      el "h1" editStringText

      el "h3" $ trText Edit_Name
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
        pure $ leftmost [ Nothing <$  cancelled
                        , Just    <$> tag editValue confirmed
                        ]

