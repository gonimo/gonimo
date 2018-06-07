{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Reflex.Class.Extended
Description : Some convenience functions missing from Reflex.
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Class.Extended ( -- * Re-exported modules
                               module Reflex
                               -- * Functions
                             , ffilter_
                             , leftmostList
                             , mergeAsList
                             , Flattenable(..)
                             , SwitchHold
                             , flatten
                             , flattenDynamic
                             , networkViewFlatten
                             , networkViewFlattenPair
                             , waitAndFilter
                             , withKey
                             , withKey_
                             , withKeyMay
                             , withKeyMay_
                             ) where


import           Control.Monad             ((<=<))
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Default
import           Data.Maybe
import           Reflex.Class              as Reflex
import           Reflex.Network
import           Reflex.NotReady.Class
import           Reflex.PostBuild.Class



-- | Variant of ffilter that discards the event's value.
ffilter_ :: Reflex t => (a -> Bool) -> Event t a -> Event t ()
ffilter_ p = fmap (const ()) . ffilter p

-- | Same as `withKey` but `val` gets discarded.
withKey_ :: forall t m key val. (Reflex t, MonadHold t m)
         => Dynamic t (Maybe key) -> Event t val -> m (Event t key)
withKey_ keyDyn inEv = fmap fst <$> withKey keyDyn inEv


-- | Forward an event with a given key stored in a Dynamic.
--
--   The key may be `Nothing` in this case, the `updated` event of the `Dynamic`
--   will be forwarded, once it becomes `Just` iff the input event happend
--   during the time it was `Nothing`.
withKey :: forall t m key val. (Reflex t, MonadHold t m)
        => Dynamic t (Maybe key) -> Event t val -> m (Event t (key, val))
withKey keyDyn inEv = do
  let
    unHandledEv = fmap snd  . ffilter (isNothing . fst) . attach (current keyDyn) $ inEv
  evHappened <- holdDyn Nothing $ leftmost [ Just <$> unHandledEv
                                           , Nothing <$ ffilter isJust (updated keyDyn)
                                           ]
  let
    onEv = push (\val -> runMaybeT $ do
                    key <- MaybeT . sample . current $ keyDyn
                    pure (key, val)
                ) inEv
    onReady = push (\mKey -> runMaybeT $ do
                      val <- MaybeT . sample . current $ evHappened
                      key <- MaybeT . pure $ mKey
                      pure (key, val)
                   ) (updated keyDyn)
  pure $ leftmost [ onEv, onReady ]
  -- Don't use the code below! The Applicative instance of Dynamic does relate
  -- the two updated events with each other if they happen at the same time,
  -- instead of sampling the other event. So the code below is not equivalent to
  -- the code above!
  -- pure
  --   . fmapMaybe id . updated . runMaybeT
  --   $ liftA2 (,) (MaybeT keyDyn) (MaybeT evHappened)


-- | Similar to withKey, but simply drops the event if the key behavior holds a `Nothing`.
withKeyMay :: forall t key val. Reflex t
           => Behavior t (Maybe key) -> Event t val -> Event t (key, val)
withKeyMay keyBeh inEv = fmapMaybe id . fmap unwrap . attach keyBeh $ inEv
  where
    unwrap :: forall a b. (Maybe a, b) -> Maybe (a, b)
    unwrap (Nothing, _) = Nothing
    unwrap (Just a, b) = Just (a, b)


withKeyMay_ :: forall t key val. Reflex t
            => Behavior t (Maybe key) -> Event t val -> Event t key
withKeyMay_ keyBeh inEv = fst <$> withKeyMay keyBeh inEv

-- | Filter event occurrences based on the `Bool` in the `Dynamic`.
--
--   If the `Dynamic` holds a `Nothing` any event occurrence will be delayed
--   until the `Dynamic` becomes "Just True". If the `Dynamic` becomes "Just
--   False", any occurred event will be dropped. This function only has a queue
--   of size one, that means if the event occurs more than once while the
--   `Dynamic` is `Nothing`, then only the last occurrence will be passed on on
--   "Just True".
--
--   Example:
--
--  >   waitAndFilter (fmap null <$> model ^. families)
--
--   Waits until some `families` Dynamic is ready (isJust), then filters the
--   event on whether or not the contained list is empty.
waitAndFilter :: forall t a m
  . (Reflex t, MonadHold t m)
  => Dynamic t (Maybe Bool) -> Event t a -> m (Event t a)
waitAndFilter waitFor onInput = fmap snd . ffilter fst <$> withKey waitFor onInput


-- | Uses the leftmost event in case of coincidence, but wraps it in a list for
--   use in an API that expects a list of events.
leftmostList :: Reflex t => [Event t a] -> Event t [a]
leftmostList = fmap (:[]) . leftmost


-- | Merge a list of events into a single list event.
--
mergeAsList :: (Functor f, Monoid (f [a]))
            => [f a] -> f [a]
mergeAsList = mconcat . map (fmap (:[]))


-- | Can be either 'switchHold never' or 'switchHoldPromptly never'
type SwitchHold = forall t a m. (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)

class Flattenable a where
  -- | The first parameter is either switchHold or switchHoldPromptly.
  --
  -- So we get both variants for free, if implementors use this parameter for
  -- implementing flattenWith.
  flattenWith :: forall t m. (Reflex t, MonadHold t m)
                  => SwitchHold -> Event t (a t) -> m (a t)


-- | Extract a type from an event, with the given initial value.
flatten :: forall a t m. (Flattenable a, Reflex t, MonadHold t m)
              => Event t (a t) -> m (a t)
flatten = flattenWith switchHold

-- | Flatten a Dynamic
flattenDynamic :: forall a t m. (Reflex t, MonadHold t m, Default a)
               => SwitchHold -> Event t (Dynamic t a) -> m (Dynamic t a)
flattenDynamic doSwitch ev = do
  let
    initVal = pushAlways (sample . current) ev
  updateVal <- doSwitch initVal (updated <$> ev)
  holdDyn def updateVal

-- | networkView combined with flattenDef
networkViewFlatten :: ( Reflex t, NotReady t m, Adjustable t m, PostBuild t m
                  , Flattenable a, MonadHold t m)
               => Dynamic t (m (a t)) -> m (a t)
networkViewFlatten = flatten <=< networkView

-- | Flatte pairs of `Flattenable`s.
networkViewFlattenPair :: ( Reflex t, NotReady t m, Adjustable t m, PostBuild t m
                          , Flattenable a, Flattenable b, MonadHold t m
                          )
               => Dynamic t (m (a t, b t)) -> m (a t, b t)
networkViewFlattenPair dynAction = do
  wrapped <- networkView dynAction
  (,) <$> flatten (fst <$> wrapped) <*> flatten (snd <$> wrapped)
