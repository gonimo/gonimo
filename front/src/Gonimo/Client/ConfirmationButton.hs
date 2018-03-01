{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.ConfirmationButton where

import           Data.Map                              (Map)
import           Data.Text                             (Text)
import           Reflex.Dom.Core

import           Gonimo.Client.ConfirmationButton.I18N
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom

data Confirmed = No | Yes

confirmationButton :: forall model t m. GonimoM model t m
                      => Map Text Text -> m () -> m () -> m (Event t ())
confirmationButton attrs inner = confirmationEl (buttonAttr attrs inner)

confirmationEl :: forall model t m. GonimoM model t m
                      => m (Event t ()) -> m () -> m (Event t ())
confirmationEl someButton confirmationText = addConfirmation confirmationText =<< someButton


mayAddConfirmation :: forall model t m. GonimoM model t m
                      => m () -> Event t () -> Dynamic t Bool -> m (Event t ())
mayAddConfirmation confirmationText clicked needsConfirmation = do
  let
    go False = pure clicked
    go True  = addConfirmation confirmationText clicked
  evEv <- dyn $ go <$> needsConfirmation
  switchPromptly never evEv

addConfirmation :: forall model t m. GonimoM model t m
                      => m () -> Event t () -> m (Event t ())
addConfirmation confirmationText clicked = mdo
  confirmationDialog <- holdDyn (pure never) $ leftmost [ const (confirmationBox confirmationText) <$> clicked
                                                        , const (pure never) <$> gotAnswer
                                                        ]
  gotAnswer <- switchPromptly never =<< dyn confirmationDialog
  pure $ push (\answer -> case answer of
                            No  -> pure Nothing
                            Yes -> pure $ Just ()
              ) gotAnswer


confirmationBox :: forall model t m. GonimoM model t m => m () -> m (Event t Confirmed)
confirmationBox confirmationText = do
  elClass "div" "fullScreenOverlay" $ do
    elClass "div" "container" $ do
      noClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
      el "h1" $ trText Are_you_sure
      el "br" blank
      el "br" blank

      confirmationText
      el "br" blank
      el "br" blank

      yesClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText OK
      pure $ leftmost [ const No <$> noClicked, const Yes <$> yesClicked ]

