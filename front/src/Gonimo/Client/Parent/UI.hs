{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Parent.UI where

import           Control.Lens
import           Data.Monoid
import qualified Data.Text                         as T
import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.DeviceList          as DeviceList
import           Gonimo.Db.Entities                (DeviceId)
import           Reflex.Dom.Core
import           Data.Foldable

import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Gonimo.Client.App.Types           as App
-- import           Gonimo.Client.Parent.Internal
import qualified Gonimo.Client.NavBar              as NavBar
import qualified Gonimo.Client.Parent.Connections  as C
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server              (webSocket_recv)
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Invite as Invite
-- import           Gonimo.Client.ConfirmationButton  (confirmationButton)

data ParentScreen = ScreenStart | ScreenRunning

ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> m (App.Screen t)
ui appConfig loaded deviceList =
  elClass "div" "container" $ mdo
      menu
      connections' <- C.connections $ C.Config { C._configResponse = appConfig^.App.server.webSocket_recv
                                              , C._configAuthData = loaded^.App.authData
                                              , C._configConnectBaby = devicesUI^.DeviceList.uiConnect
                                              -- , C._configDisconnectAll  = leftmost [uiDisconnect, uiGoHome]
                                              , C._configDisconnectAll = never
                                              , C._configDisconnectBaby = devicesUI^.DeviceList.uiDisconnect
                                              }
      devicesUI <- DeviceList.ui loaded deviceList (Set.fromList . Map.keys <$> connections'^.C.streams)

      -- uiConnectBabyEv <- dyn $ connectButtons . Map.keys <$> deviceList^.DeviceList.onlineDevices
      -- uiConnectBaby <- switchPromptly never uiConnectBabyEv
      -- uiDisconnect <- buttonAttr ("class" =: "btn btn-default") $ text "Disconnect"
      -- uiGoHome <- buttonAttr ("class" =: "btn btn-default") $ text "Go to your mummy"

      let streams = Map.elems <$> connections'^.C.streams
      _ <- dyn $ renderVideos <$> streams

      invite <- 
        Invite.ui $ Invite.Config { Invite._configResponse = appConfig^.App.server.webSocket_recv
                                  , Invite._configSelectedFamily = loaded^.App.selectedFamily
                                  , Invite._configAuthenticated = appConfig^.App.auth.Auth.authenticated
                                  , Invite._configCreateInvitation = never
                                  }

      emptySubs <- holdDyn mempty never
      let parentApp = App.App { App._subscriptions = emptySubs
                              , App._request = connections'^.C.request <> devicesUI^.DeviceList.uiRequest <> invite^.Invite.request
                              }
      pure $ App.Screen { App._screenApp = parentApp
                        , App._screenGoHome = never
                        }


connectButtons :: forall m t. (HasWebView m, MonadWidget t m) => [DeviceId] -> m (Event t DeviceId)
connectButtons devIds = do
  let
    mkBtn devId = do
      clicked <- buttonAttr ("class" =: "btn btn-default") . text . T.pack . show $ devId
      pure $ const devId <$> clicked
  buttons <- traverse mkBtn devIds
  pure $ leftmost buttons



renderVideos :: forall m t. (HasWebView m, MonadWidget t m) => [MediaStream] -> m ()
renderVideos streams = do
  let
    renderVideo stream
      = mediaVideo stream ("autoplay" =: "true")
  traverse_ renderVideo streams

menu :: forall m t. (HasWebView m, MonadWidget t m) => m ()
menu = do
  elClass "div" "menu" $ do
    elClass "div" "menu-left back" blank
    elClass "div" "menu-left home" blank
    elClass "div" "menu-right home" $ do
      text "Der Geraet ist sehr cool und so"
      el "br" blank
      text "Red Dog"
