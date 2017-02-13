{-# LANGUAGE CPP #-}
-- Common imports and definitions for frontend ...
module Gonimo.Client.Prelude ( MonadFix
                             , module GonimoPrelude
                             ) where

import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT)
#ifndef __GHCJS__
import           GHCJS.DOM.Types           (MonadJSM (..), liftJSM)
#endif
import           Gonimo.Prelude as GonimoPrelude

-- Only needed on GHC, because on GHCJS MonadJSM is MonadIO
#ifndef __GHCJS__
instance MonadJSM m => MonadJSM (MaybeT m) where
  liftJSM' = lift . liftJSM
#endif
