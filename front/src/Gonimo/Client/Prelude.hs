-- Common imports and definitions for frontend ...
module Gonimo.Client.Prelude where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Class (lift)
import           GHCJS.DOM.Types (MonadJSM(..), liftJSM)

instance MonadJSM m => MonadJSM (MaybeT m) where
  liftJSM' = lift . liftJSM
