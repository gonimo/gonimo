{-|
Module      : Behavior
Description : Missing functionality for behaviors
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Behavior ( foldp
                       ) where



import Reflex.Class
import Control.Monad.Fix

-- | Fold the past with some event to gain the future.
foldp :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Behavior t b)
foldp = accumB . flip



