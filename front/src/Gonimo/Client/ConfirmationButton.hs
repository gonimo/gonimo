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
mayAddConfirmation confirmationText clicked needsConfirmation =
  fmapMaybe (^? _Yes) <$> mayAddConfirmation' confirmationText clicked needsConfirmation

-- | Get confirmation for given event.
mayAddConfirmation' :: forall model t m. GonimoM model t m
                      => m () -> Event t () -> Dynamic t Bool -> m (Event t Confirmed)
mayAddConfirmation' confirmationText clicked needsConfirmation = do
  let
    go False = pure $ Yes <$ clicked
    go True  = addConfirmation' confirmationText clicked
  evEv <- dyn $ go <$> needsConfirmation
  switchHold never evEv

addConfirmation :: forall model t m. GonimoM model t m
                      => m () -> Event t () -> m (Event t ())
addConfirmation confirmationText clicked
  = fmapMaybe (^? _Yes) <$> addConfirmation' confirmationText clicked

addConfirmation' :: forall model t m. GonimoM model t m
                      => m () -> Event t () -> m (Event t Confirmed)
addConfirmation' confirmationText clicked = mdo
  confirmationDialog <- holdDyn (pure never) $ leftmost [ const (confirmationBox confirmationText) <$> clicked
                                                        , const (pure never) <$> gotAnswer
                                                        ]
  gotAnswer <- switchHold never =<< dyn confirmationDialog
  pure gotAnswer


confirmationBox :: forall model t m. GonimoM model t m => m () -> m (Event t Confirmed)
confirmationBox confirmationText = do
  elClass "div" "fullScreenOverlay overlay-most-important" $ do
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

-- Prisms:

_No :: Prism' Confirmed ()
_No
  = prism
        (\ () -> No)
        (\ x
           -> case x of
                No -> Right ()
                _ -> Left x)

_Yes :: Prism' Confirmed ()
_Yes
  = prism
        (\ () -> Yes)
        (\ x
           -> case x of
                Yes -> Right ()
                _ -> Left x)
