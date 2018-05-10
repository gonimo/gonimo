{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
-- Common imports and definitions for frontend ...
{-# OPTIONS_GHC -Wno-orphans #-}
module Gonimo.Client.Prelude ( MonadFix
                             , module GonimoPrelude
                             , module Settings
                             , GonimoM
                             , module Reflex.Dom.Class
                             , module Reflex.Class.Extended
                             , module Reflex.Dynamic.Extended
                             , module Reflex
                             , module MonadReader
                             , module GHCJS.DOM.Types
                             ) where

import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT)
#ifndef __GHCJS__
import           GHCJS.DOM.Types            (MonadJSM (..), liftJSM)
#else
import           GHCJS.DOM.Types            (MonadJSM, liftJSM)
#endif
import           Control.Monad.Reader.Class
import           Gonimo.Client.Settings     as Settings (HasSettings (..),
                                                         trDynText, trText)
import           Gonimo.Prelude             as GonimoPrelude
import           Reflex
import           Reflex.Class.Extended
import           Reflex.Dom.Class           hiding (Alt)
import           Reflex.Dom.Core
import           Reflex.Dynamic.Extended
import           Control.Monad.Reader.Class as MonadReader (ask)


-- | Constraint on a Monad m, to be suitable for Gonimo UI code.
--
--   This also includes a 'MonadReader' on a Model holding user settings, so
--   functions like 'trText' work properly.
type GonimoM model t m = ( DomBuilder t m , PostBuild t m , TriggerEvent t m
                   , MonadJSM m, MonadHold t m, MonadFix m
                   , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
                   , MonadIO (Performable m), PerformEvent t m
                   , MonadSample t (Performable m)
                   , HasWebView m
                   , HasSettings model
                   , MonadReader (model t) m
                   )

