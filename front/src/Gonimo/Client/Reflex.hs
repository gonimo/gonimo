{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex where

import Reflex.Dom.Core
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))


-- | Only pass through every second instance of an event.
--
--   Events [1, 2, 3, 4] would result in events [ 2, 4 ].
everySecond :: forall t m a. (MonadHold t m, Reflex t , MonadFix m) => Event t a -> m (Event t a)
everySecond ev = do
  isValid <- toggle False $ ev
  pure $ push (\a -> do
                 isValid' <- sample $ current isValid
                 if isValid'
                   then pure $ Just a
                   else pure Nothing
              )
              ev

buildDynMap :: (Ord k, Eq k, Reflex t) => Behavior t (Map k v) -> [Event t (Map k v -> Map k v)] -> Event t (Map k v)
buildDynMap bMap = pushAlways (\builder -> do
                                  cMap <- sample bMap
                                  pure $ builder cMap
                              ) . mergeWith (.)

-- class FlattenAble a where
--   flattenEvent ::  forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t a -> m a

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
                => m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Event t b)
fromMaybeDyn onNothing action mDyn = dyn =<< fromMaybeDyn' onNothing action mDyn

fromMaybeDyn' :: (DomBuilder t m, MonadHold t m, Show a)
                => m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Dynamic t (m b))
fromMaybeDyn' onNothing action mDyn = do
  mInit <- sample $ current mDyn
  let onlyJusts = push (pure . id) $ updated mDyn
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


buildMap :: forall m t key val . (MonadFix m, Reflex t, MonadHold t m, Ord key)
                 => Dynamic t [key] -> Event t (key, val)
                 -> m (Dynamic t (Map key (Dynamic t val)))
buildMap keys gotNewKeyVal' = mdo
  let
    gotNewKeyVal = push (\p@(k,_) -> do
                            cKeys <- sample $ current keys
                            if k `elem` cKeys
                              then pure $ Just p
                              else pure Nothing
                        ) gotNewKeyVal'
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

  resultMap <- holdDyn Map.empty . buildDynMap (current resultMap) $ [ insertKeys, deleteKeys ]
  pure resultMap
