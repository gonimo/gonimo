{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.App.Internal where

import Reflex.Dom
import Data.Monoid
import Data.Text (Text)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Data.Maybe (isJust, isNothing)
import Safe (headMay)
import Data.List (sort)
import Gonimo.Client.Reflex
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative
import Control.Monad.Fix (MonadFix)

import Gonimo.Client.App.Types

appSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (App t) -> m (App t)
appSwitchPromptly ev
  = App <$> makeReady Set.empty (_subscriptions <$> ev)
        <*> switchPromptly never (_request <$> ev)

