{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex where

import Reflex.Dom
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, (\\))

-- Build an event that only triggers on the very first occurrence of the input event.
waitForReady :: forall t m a. (MonadHold t m, Reflex t, MonadFix m) => Event t a -> m (Event t (Dynamic t a))
waitForReady inEv = do
  onFirst <- headE inEv
  pure $ push (\val -> Just <$> holdDyn val inEv) onFirst

-- Does not work as expected, see below for a working version.
-- TODO: Understand why this does not work!
-- -- "Inverse" of waitForReady, extract a dynamic from an event.
-- makeReady :: forall t m a. (MonadHold t m, Reflex t, MonadFix m) => a -> Event t (Dynamic t a) -> m (Dynamic t a)
-- makeReady startVal dynEv = do
--   let getCurrent = push (\dynVal -> fmap Just (sample $ current dynVal)
--                         ) dynEv
--   let getUpdated = updated <$> dynEv
--   updates <- switchPromptly getCurrent getUpdated
--   holdDyn startVal updates


-- "Inverse" of waitForReady, extract a dynamic from an event.
makeReady :: forall t m a. (MonadHold t m, Reflex t, MonadFix m) => a -> Event t (Dynamic t a) -> m (Dynamic t a)
makeReady startVal dynEv = do
  startDyn <- holdDyn startVal never
  nested <- holdDyn startDyn dynEv
  pure $ join nested


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

fromMaybeDyn :: (DomBuilder t m, PostBuild t m, MonadSample t m, MonadHold t m, Show a)
                => (String -> String) -> m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Event t b)
fromMaybeDyn myTag onNothing action mDyn = dyn =<< fromMaybeDyn' myTag onNothing action mDyn

fromMaybeDyn' :: (DomBuilder t m, MonadHold t m, Show a)
                => (String -> String) -> m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Dynamic t (m b))
fromMaybeDyn' myTag onNothing action mDyn = do
  mInit <- sample $ current mDyn
  let onlyJusts = traceEvent (myTag "got Just update") . push (pure . id) . traceEvent (myTag "got update") $ (updated mDyn)
  let widgetInit = case mInit of
        Nothing -> trace (myTag "onNothing in mInit") onNothing
        Just v  -> trace (myTag "action in mInit") $ action =<< holdDyn v onlyJusts
  let widgetEvent = push (\mVal -> do
                             prevMVal <- trace (myTag "Got sample ...") . sample $ current mDyn
                             pure $ case (prevMVal, mVal) of
                               (_, Nothing)        -> trace (myTag "Returning onNothing") $ Just onNothing
                               (Nothing, Just val) -> trace (myTag "Returning action on Just") $ Just (action =<< holdDyn val onlyJusts)
                               (Just _, Just _)    -> trace (myTag "Returning Nothing") Nothing
                         ) (updated mDyn)
  holdDyn widgetInit widgetEvent


buildMap :: forall m t key val . (MonadFix m, Reflex t, MonadHold t m, Ord key)
                 => Dynamic t [key] -> Event t (key, val)
              -> m (Dynamic t (Map key (Dynamic t val)))
buildMap keys gotNewKeyVal = mdo
  let
    gotNewVal :: key -> Event t val
    gotNewVal key' = push (\(k,v)
                           -> pure $ if k == key'
                                     then Just v
                                     else Nothing
                          ) gotNewKeyVal

    insertKeys :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val))
    insertKeys = push (\(key, val) -> do
                          oldMap <- sample $ current resultMap
                          if (Map.member key oldMap)
                          then
                            pure Nothing -- Nothing to do.
                          else do
                            dynVal <- holdDyn val (gotNewVal key)
                            pure . Just $ Map.insert key dynVal
                      ) gotNewKeyVal

    deleteKeys :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val))
    deleteKeys = push (\keys' -> do
                         oldKeys <- fmap Set.fromList . sample $ current keys
                         let newKeys = Set.fromList keys'
                         let deletedKeys = oldKeys \\ newKeys
                         if Set.null deletedKeys
                           then pure Nothing
                           else pure $ Just $ \oldMap -> foldr Map.delete oldMap deletedKeys
                     ) (updated keys)

    updateMap :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val)) -> Event t (Map key (Dynamic t val))
    updateMap = push (\updateF -> do
                         oldMap <- sample $ current resultMap
                         pure $ Just $ updateF oldMap
                     )
  resultMap <- holdDyn Map.empty . updateMap $ mergeWith (.) [ insertKeys, deleteKeys ]
  pure resultMap
