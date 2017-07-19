{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
-- Common imports and definitions for frontend ...
{-# OPTIONS_GHC -Wno-orphans #-}
module Gonimo.Client.Prelude ( MonadFix
                             , module GonimoPrelude
                             , module I18N
                             , GonimoM
                             ) where

import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT)
#ifndef __GHCJS__
import           GHCJS.DOM.Types           (MonadJSM (..), liftJSM)
#else
import           GHCJS.DOM.Types           (MonadJSM)
#endif
import           Gonimo.Prelude as GonimoPrelude
import           Reflex.Dom.Core
import           Gonimo.Client.I18N as I18N (GonimoEnv, trText, trDynText)
import Control.Monad.Reader.Class


type GonimoM t m = ( DomBuilder t m , PostBuild t m , TriggerEvent t m
                   , MonadJSM m, MonadHold t m, MonadFix m
                   , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
                   , MonadIO (Performable m), PerformEvent t m
                   , MonadSample t (Performable m)
                   , HasWebView m
                   , MonadReader (GonimoEnv t) m
                   )
