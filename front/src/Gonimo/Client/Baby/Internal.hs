{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.Client.Baby.Internal where

import Reflex.Dom
import Data.Monoid
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens 
import qualified GHCJS.DOM.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Safe (headMay)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative
import Gonimo.Client.Server (webSocket_recv)

import Gonimo.Client.Subscriber (SubscriptionsDyn)
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.Auth as Auth
import Gonimo.DOM.Navigator.MediaDevices

type Config t = ()

data Baby t
  = Baby { _videoDevices :: [MediaDeviceInfo]
         }


makeLenses ''Baby

baby :: forall m t. (MonadWidget t m) => Config t -> m (Baby t)
baby _ = do
  devices <- enumerateDevices
  let videoDevices' = filter ((== VideoInput) . mediaDeviceKind) devices
  pure $ Baby videoDevices'

