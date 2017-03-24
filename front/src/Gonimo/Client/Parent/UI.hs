{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Parent.UI where

import           Control.Lens
import           Data.Monoid
import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.DeviceList          as DeviceList
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
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Invite as Invite
import           Gonimo.Client.ConfirmationButton  (mayAddConfirmation)
import           Gonimo.Client.Util (volumeMeter)


ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> m (App.Screen t)
ui appConfig loaded deviceList = mdo
  connections' <- C.connections $ C.Config { C._configResponse = appConfig^.App.server.webSocket_recv
                                           , C._configAuthData = loaded^.App.authData
                                           , C._configConnectBaby = devicesUI^.DeviceList.uiConnect
                                           , C._configDisconnectAll = leftmost [ navBar^.NavBar.backClicked
                                                                               , navBar^.NavBar.homeClicked
                                                                               , viewNav^.NavBar.homeClicked
                                                                               ]
                                           , C._configDisconnectBaby = devicesUI^.DeviceList.uiDisconnect
                                           }

  let showParentView = const "isParentView" <$> leftmost [ devicesUI^.DeviceList.uiConnect
                                                         , devicesUI^.DeviceList.uiShowStream
                                                         ]
  let showParentManage = const "isParentManage" <$> leftmost [ viewNav^.NavBar.backClicked
                                                             , invite^.Invite.uiGoBack
                                                             , invite^.Invite.uiDone
                                                             ]
  let showInviteView = const "isInviteView" <$> inviteRequested

  selectedView <- holdDyn "isParentManage" $ leftmost [showParentView, showParentManage, showInviteView]

  (navBar, devicesUI, inviteRequested) <-
    elDynClass "div" (pure "container parentManage " <> selectedView) $ do
      manageUi appConfig loaded deviceList connections'

  viewNav <-
    elDynClass "div" (pure "container parentView " <> selectedView) $ do
      viewUi appConfig loaded deviceList connections'

  invite <-
    elDynClass "div" (pure "container inviteView " <> selectedView) $ do
      firstCreation <- headE inviteRequested
      let inviteUI
            = Invite.ui loaded
              $ Invite.Config { Invite._configResponse = appConfig^.App.server.webSocket_recv
                              , Invite._configSelectedFamily = loaded^.App.selectedFamily
                              , Invite._configAuthenticated = appConfig^.App.auth.Auth.authenticated
                              , Invite._configCreateInvitation = never
                              }
      dynInvite <- widgetHold (pure def) $ const inviteUI <$> firstCreation
      pure $ Invite.inviteSwitchPromptlyDyn dynInvite

  emptySubs <- holdDyn mempty never
  let parentApp = App.App { App._subscriptions = emptySubs
                          , App._request = connections'^.C.request <> devicesUI^.DeviceList.uiRequest <> invite^.Invite.request
                          }
  pure $ App.Screen { App._screenApp = parentApp
                    , App._screenGoHome = leftmost [ navBar^.NavBar.backClicked
                                                   , navBar^.NavBar.homeClicked
                                                   , viewNav^.NavBar.homeClicked
                                                   ]
                    }

manageUi :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> C.Connections t -> m (NavBar.NavBar t, DeviceList.UI t, Event t ())
manageUi _ loaded deviceList connections' = do
      navBar <- NavBar.navBar (NavBar.Config loaded deviceList)
      let openStreams = connections'^.C.streams
      navBar' <- NavBar.NavBar
                 <$> mayAddConfirmation leaveConfirmation (navBar^.NavBar.backClicked) (not . Map.null <$> openStreams)
                 <*> mayAddConfirmation leaveConfirmation (navBar^.NavBar.homeClicked) (not . Map.null <$> openStreams)
      devicesUI <- DeviceList.ui loaded deviceList (Set.fromList . Map.keys <$> openStreams)
      inviteRequested <-
            makeClickable . elAttr' "div" (addBtnAttrs "device-add") $ text " Add Device"

      pure (navBar', devicesUI, inviteRequested)

viewUi :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> C.Connections t -> m (NavBar.NavBar t)
viewUi _ loaded deviceList connections = do
  let streams = connections^.C.streams
  let origStreams = connections^.C.origStreams
  navBar <- NavBar.navBar (NavBar.Config loaded deviceList)

  navBar' <- NavBar.NavBar (navBar^.NavBar.backClicked)
             <$> mayAddConfirmation leaveConfirmation (navBar^.NavBar.homeClicked) (not . null <$> streams)
  elClass "div" "parent" $ do
    _ <- dyn $ renderVideos True . Map.elems <$> origStreams
    _ <- dyn $ renderVideos False . Map.elems <$> streams
    pure navBar'


renderVideos :: forall m t. (HasWebView m, MonadWidget t m) => Bool -> [MediaStream] -> m ()
renderVideos fakeRender streams = do
  let
    renderFake stream = mediaVideo stream ("autoplay" =: "true" <> "style" =: "width:100%;height:100%;" <> "class" =: "fakeVideo" <> "muted" =: "true")
    renderVideo stream = elClass "div" "stream-baby" $ do
      mediaVideo stream ("autoplay" =: "true" <> "style" =: "width:100%;height:100%;")
      volumeMeter stream
    renderFakeOrNot = if fakeRender
      then renderFake
      else renderVideo
  traverse_ renderFakeOrNot streams


leaveConfirmation :: DomBuilder t m => m ()
leaveConfirmation = do
    el "h3" $ text "Really stop parent station?"
    el "p" $ text "All open streams will be disconnected!"
