{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex where

import Reflex.Dom
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)


-- Build an event that only triggers on the very first occurrence of the input event.
waitForReady :: forall t m a. (MonadHold t m, Reflex t, MonadFix m) => Event t a -> m (Event t (Dynamic t a))
waitForReady inEv = mdo
  hasOccurred <- holdDyn False (const True <$> outEv)
  let
    outEv = push (\val -> do
                  hasOccurred' <- sample $ current hasOccurred
                  if hasOccurred'
                  then pure Nothing
                  else Just <$> holdDyn val inEv
              ) inEv
  pure outEv


-- | Wait for first just, then only update on new Just values.
waitForJust :: forall t m a. (MonadHold t m, Reflex t, MonadFix m, TriggerEvent t m, MonadIO m)
               => Dynamic t (Maybe a) -> m (Event t (Dynamic t a))
waitForJust dynMay = do
  mC <- sample $ current dynMay
  case mC of
    Nothing -> waitForReady . push (pure . id) . updated $ dynMay
    Just c -> do
      (ev, makeEv) <- newTriggerEvent
      outDyn <- holdDyn c $ push (pure . id) (updated dynMay)
      liftIO $ makeEv outDyn
      pure ev

fromMaybeDyn :: (DomBuilder t m, PostBuild t m, MonadSample t m, MonadHold t m)
                => m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Event t b)
fromMaybeDyn onNothing action mDyn = dyn =<< fromMaybeDyn' onNothing action mDyn

fromMaybeDyn' :: (DomBuilder t m, PostBuild t m, MonadSample t m, MonadHold t m)
                => m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Dynamic t (m b))
fromMaybeDyn' onNothing action mDyn = do
  mInit <- sample $ current mDyn
  let onlyJusts = push (pure . id) (updated mDyn)
  let widgetInit = case mInit of
        Nothing -> onNothing
        Just v  -> action =<< holdDyn v onlyJusts
  let widgetEvent = push (\mVal -> do
                             prevMVal <- sample $ current mDyn
                             pure $ case (prevMVal, mVal) of
                               (_, Nothing)        -> Just onNothing
                               (Nothing, Just val) -> Just (action =<< holdDyn val onlyJusts)
                               (Just _, Just _)    -> Nothing
                         ) (updated mDyn)
  holdDyn widgetInit widgetEvent
