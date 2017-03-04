{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.App.UI where

import           Control.Lens
import           Data.Monoid
import qualified Data.Set                       as Set
import qualified Gonimo.Client.DeviceList       as DeviceList
import           Gonimo.Client.Reflex
import           Reflex.Dom.Core

import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import           Gonimo.Client.App.Internal
import           Gonimo.Client.App.Types
import qualified Gonimo.Client.Auth             as Auth
import qualified Gonimo.Client.Family           as Family
import qualified Gonimo.Client.MessageBox       as MessageBox
import           Gonimo.Client.Server           (webSocket_recv)
import qualified Gonimo.Client.Baby             as Baby
import qualified Gonimo.Client.Parent           as Parent
import           Gonimo.Client.Reflex.Dom


ui :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> m (App t)
ui config = elClass "div" "app" $ mdo
  family <- Family.family
            $ (Family.fromApp config) & Family.configSelectFamily .~ leftmost [ msgSwitchFamily
                                                                              , familyUI^.Family.uiSelectFamily
                                                                              ]
                                      & Family.configCreateFamily .~ familyUI^.Family.uiCreateFamily
                                      & Family.configLeaveFamily .~ familyUI^.Family.uiLeaveFamily
                                      & Family.configSetName .~ familyUI^.Family.uiSetName

  -- navEv <- navBar family
  -- let navCreateFamily = push (pure . (^?_Left)) navEv
  -- let navSelectFamily = push (pure . (^?_Right)) navEv

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

  let onReady dynAuthFamilies =
        fromMaybeDyn
          (do
              clickedAdd <- buttonAttr ("class" =: "btn btn-default") $ text "+"
              elClass "div" "container" $ text "Create a family to get started (+)"
              subs <- holdDyn Set.empty never
              pure (App subs never, Family.UI never clickedAdd never never never)
          )
          (\selected -> do
              let loaded = Loaded (fst <$> dynAuthFamilies) (snd <$> dynAuthFamilies) selected
              loadedUI config loaded
          )
          (family^.Family.selectedFamily)
  let notReady = do
        elClass "div" "container" $ text "Loading, stay tight..."
        pure never
  dynEvEv <- widgetHold notReady (onReady <$> evReady)

  let evEv = switchPromptlyDyn dynEvEv -- Flatten Dynamic Event Event
  (,) <$> appSwitchPromptly (fst <$> evEv)
      <*> Family.uiSwitchPromptly (snd <$> evEv)


loadedUI :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> Loaded t -> m (App t, Family.UI t)
loadedUI config loaded = mdo
  deviceList <- DeviceList.deviceList $ DeviceList.Config { DeviceList._configResponse = config^.server.webSocket_recv
                                                          , DeviceList._configAuthData = config^.auth.Auth.authData
                                                          , DeviceList._configFamilyId = loaded^.selectedFamily
                                                          }
  emptyScreen <- mkEmptyScreen
  dynPair <- widgetHold
             ((emptyScreen,) <$> Family.ui config loaded)
             (renderCenter config loaded deviceList <$> roleSelected)

  let screen = screenSwitchPromptlyDyn . fmap fst $ dynPair
  let familyUI = Family.uiSwitchPromptlyDyn . fmap snd $ dynPair

  let roleSelected = leftmost [ Just <$> familyUI^.Family.uiRoleSelected
                              , const Nothing <$> screen^.screenGoHome
                              ]

  let app = App { _request = deviceList^.DeviceList.request <> screen^.screenApp.request
                , _subscriptions = deviceList^.DeviceList.subscriptions <> screen^.screenApp.subscriptions
                }
  pure (app, familyUI)


-- Returned pair: Cancel event (go back to family screen and Family.UI)
renderCenter :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> Loaded t -> DeviceList.DeviceList t -> Maybe Family.GonimoRole -> m (Screen t, Family.UI t)
renderCenter config loaded deviceList mRole = do
  emptyScreen <- mkEmptyScreen
  case mRole of
      Nothing -> (emptyScreen,) <$> Family.ui config loaded
      Just Family.RoleBaby -> (, def) <$> Baby.ui config loaded deviceList
      Just Family.RoleParent -> (,def) <$> Parent.ui config loaded deviceList
