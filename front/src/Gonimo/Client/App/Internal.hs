{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.App.Internal where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import qualified Data.Set as Set
import Gonimo.Client.Reflex

import Gonimo.Client.App.Types

appSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (App t) -> m (App t)
appSwitchPromptly ev
  = App <$> makeReady Set.empty (_subscriptions <$> ev)
        <*> switchHoldPromptly never (_request <$> ev)
        <*> switchHoldPromptly never (_selectLang <$> ev)

