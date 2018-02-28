{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Reflex.Dynamic.Extended
Description : Some convenience functions missing from Reflex.
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Dynamic.Extended ( -- * Re-exported modules
                                 module Reflex
                               -- * Functions
                               , holdDynUniq
                               ) where

import           Control.Monad.Fix
import           Reflex.Dynamic           as Reflex
import           Reflex.Class






-- | Convenience function which combines holdDyn with holdUniqDyn.
holdDynUniq :: (Reflex t, MonadHold t m, MonadFix m, Eq a) => a -> Event t a -> m (Dynamic t a)
holdDynUniq initial upd = holdUniqDyn =<< holdDyn initial upd
