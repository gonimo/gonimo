{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.App.UI where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.Types as Gonimo
import Control.Lens
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Gonimo.Client.Reflex
import Data.Maybe (fromMaybe)
import qualified Gonimo.Types as Gonimo
import qualified Gonimo.Client.Invite as Invite
import qualified Gonimo.Client.DeviceList as DeviceList
import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce
import Debug.Trace (trace)

import Gonimo.Client.App.Internal
import Gonimo.Client.App.Types
import qualified Gonimo.Client.Server as Server
import Gonimo.Client.Server (webSocketConfig_send, webSocket_recv, webSocket_open)
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Invite as Invite
import qualified Gonimo.Client.MessageBox as MessageBox
import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import qualified Gonimo.Client.Family as Family
import qualified Gonimo.Client.Subscriber as Subscriber

ui :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> m (App t)
ui config = mdo
  msgBox <- MessageBox.ui $ MessageBox.fromApp config

  let msgSwitchFamily = push (\actions -> case actions of
                                 [MessageBox.SelectFamily fid] -> pure $ Just fid
                                 _ -> pure Nothing -- Dirty: We ignore selectfamily if multiple events occurred ...
                             ) (msgBox ^. MessageBox.action)

  accept <- AcceptInvitation.ui $ AcceptInvitation.fromApp config
  family <- Family.ui $ (Family.fromApp config) & Family.configSelectFamily .~ msgSwitchFamily

  pure $ App { _request = accept^.AcceptInvitation.request
                       <> family^.Family.request
             , _subscriptions = family^.Family.subscriptions
             }
