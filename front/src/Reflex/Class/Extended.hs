{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Reflex.Class.Extended
Description : Some convenience functions missing from Reflex.
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Class.Extended ( -- * Re-exported modules
                               module Reflex
                               -- * Functions
                             , leftmostList
                             , mergeAsList
                             , Flattenable(..)
                             , SwitchHold
                             , flatten
                             , networkViewFlatten
                             ) where


import           Reflex.Class           as Reflex
import           Reflex.Network
import           Reflex.NotReady.Class
import           Reflex.PostBuild.Class
import Control.Monad ((<=<))






-- | Uses the leftmost event in case of coincidence, but wraps it in a list for
--   use in an API that expects a list of event.s
leftmostList :: Reflex t => [Event t a] -> Event t [a]
leftmostList = fmap (:[]) . leftmost


-- | Merge a list of events into a single list event.
--
mergeAsList :: (Functor f, Monoid (f [a]))
            => [f a] -> f [a]
mergeAsList = mconcat . map (fmap (:[]))


-- | Can be either 'switchHold never' or 'switchHoldPromptly never'
type SwitchHold = forall t a m. (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)

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
flatten = flattenWith (switchHold never)


-- | networkView combined with flattenDef
networkViewFlatten :: ( Reflex t, NotReady t m, Adjustable t m, PostBuild t m
                  , Flattenable a, MonadHold t m)
               => Dynamic t (m (a t)) -> m (a t)
networkViewFlatten = flatten <=< networkView
