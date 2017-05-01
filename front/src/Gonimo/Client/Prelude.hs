{-# LANGUAGE CPP #-}
-- Common imports and definitions for frontend ...
{-# OPTIONS_GHC -Wno-orphans #-}
module Gonimo.Client.Prelude ( MonadFix
                             , module GonimoPrelude
                             , module I18N
                             ) where

import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT)
#ifndef __GHCJS__
import           GHCJS.DOM.Types           (MonadJSM (..), liftJSM)
#endif
import           Gonimo.Prelude as GonimoPrelude
import           Reflex.Dom.Core
import           Gonimo.Client.I18N as I18N (trText, trDynText)
import           Gonimo.Client.I18N (GonimoEnv)
import Control.Monad.Reader.Class

-- Only needed on GHC, because on GHCJS MonadJSM is MonadIO
#ifndef __GHCJS__
instance MonadJSM m => MonadJSM (MaybeT m) where
  liftJSM' = lift . liftJSM
#endif


type GonimoM t m = ( DomBuilder t m
                   , PostBuild t m
                   , MonadReader (GonimoEnv t) m
                   )
