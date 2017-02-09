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
  family <- Family.family
            $ (Family.fromApp config) & Family.configSelectFamily .~ leftmost [ msgSwitchFamily, navSelectFamily ]
                                      & Family.configCreateFamily .~ familyUI^.Family.uiCreateFamily
                                      & Family.configLeaveFamily .~ familyUI^.Family.uiLeaveFamily
                                      & Family.configSetName .~ familyUI^.Family.uiSetName

  navSelectFamily <- navBar family
  msgBox <- MessageBox.ui $ MessageBox.fromApp config

  let msgSwitchFamily = push (\actions -> case actions of
                                [MessageBox.SelectFamily fid] -> pure $ Just fid
                                _ -> pure Nothing -- Dirty: We ignore selectfamily if multiple events occurred ...
                            ) (msgBox ^. MessageBox.action)

  accept <- AcceptInvitation.ui $ AcceptInvitation.fromApp config
  (app, familyUI) <- runLoaded config family
  pure $ app & request %~ (<> (  family^.Family.request
                              <> accept^.AcceptInvitation.request
                              )
                          )
             & subscriptions %~ (<> family^.Family.subscriptions)


runLoaded :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> Family.Family t -> m (App t, Family.UI t)
runLoaded config family = do
  let mkFuncPair fa fb = (,) <$> fa <*> fb
  evReady <- waitForJust
             $ zipDynWith mkFuncPair (config^.auth.Auth.authData) (family^.Family.families)

  let onReady (authData, families) =
        fromMaybeDyn
          (do
              el "div" $ text "Create a family to get started (+)"
              subs <- holdDyn Set.empty never
              pure (App subs never, Family.UI never never never never)
          )
          (\selected -> do
              let loaded = Loaded authData families selected
              loadedUI config loaded
          )
          (family^.selectedFamily)
  let notReady = do
        el "div" $ text "Loading, stay tight..."
        pure never
  dynEvEv <- widgetHold noFamilies (onLoaded <$> evReady)

  let evEv = switchPromptlyDyn dynEvEv -- Flatten Dynamic Event Event
  (,) <$> appSwitchPromptly (fst <$> evEv)
      <*> Family.uiSwitchPromptly (snd <$> evEv)


loadedUI :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> Loaded t -> m (App t, Family.UI t)
loadedUI config loaded = mdo
  deviceList <- DeviceList.deviceList $ (DeviceList.fromApp config)

  familyUI <- Family.ui config loaded deviceList

  let app = App { _request = familyUI^.Family.uiRequest
                          <> deviceList^.DeviceList.request
                , _subscriptions = deviceList^.DeviceList.subscriptions
                }
  pure (app, familyUI)


navBar :: forall m t. (HasWebView m, MonadWidget t m)
      => Family.Family t -> m (Event t Db.FamilyId)
navBar family = do
  elClass "div" "navbar navbar-default" $ do
    elClass "div" "container" $ do
      elClass "div" "navbar-header" $ navLogo
      -- elClass "div" "navbar-header" $ Family.familyChooser family
  where
    navLogo
      = elAttr "img" ( "alt" =: "gonimo"
                     <> "src" =: "pix/gonimo-brand-01.svg"
                     <> "height" =: "50px"
                     <> "style" =: "padding: 2px 3.5px 0px 3.5px;"
                     ) blank
