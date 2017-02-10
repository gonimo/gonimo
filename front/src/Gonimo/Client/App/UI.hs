{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.App.UI where

import           Control.Lens
import           Data.Monoid
import qualified Data.Set                       as Set
import qualified Gonimo.Client.DeviceList       as DeviceList
import           Gonimo.Client.Reflex
import qualified Gonimo.Db.Entities             as Db
import           Reflex.Dom

import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import           Gonimo.Client.App.Internal
import           Gonimo.Client.App.Types
import qualified Gonimo.Client.Auth             as Auth
import qualified Gonimo.Client.Family           as Family
import qualified Gonimo.Client.MessageBox       as MessageBox
import           Gonimo.Client.Server           (webSocket_recv)

ui :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> m (App t)
ui config = mdo
  family <- Family.family
            $ (Family.fromApp config) & Family.configSelectFamily .~ leftmost [ msgSwitchFamily, navSelectFamily ]
                                      & Family.configCreateFamily .~ leftmost [ familyUI^.Family.uiCreateFamily,  navCreateFamily ]
                                      & Family.configLeaveFamily .~ familyUI^.Family.uiLeaveFamily
                                      & Family.configSetName .~ familyUI^.Family.uiSetName

  navEv <- navBar family
  let navCreateFamily = push (pure . (^?_Left)) navEv
  let navSelectFamily = push (pure . (^?_Right)) navEv
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
              elClass "div" "container" $ text "Create a family to get started (+)"
              subs <- holdDyn Set.empty never
              pure (App subs never, Family.UI never never never never)
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

  familyUI <- Family.ui config loaded deviceList

  let app = App { _request = familyUI^.Family.uiRequest
                          <> deviceList^.DeviceList.request
                , _subscriptions = deviceList^.DeviceList.subscriptions
                }
  pure (app, familyUI)


navBar :: forall m t. (HasWebView m, MonadWidget t m)
      => Family.Family t -> m (Event t (Either () Db.FamilyId))
navBar family = do
  elClass "div" "navbar navbar-default" $ do
    elClass "div" "container" $ do
      elClass "div" "navbar-header" $
        navLogo
      elClass "ul" "nav navbar-nav" $ Family.familyChooser family
  where
    navLogo
      = elAttr "img" ( "alt" =: "gonimo"
                     <> "src" =: "pix/gonimo-brand-01.svg"
                     <> "height" =: "50px"
                     <> "style" =: "padding: 2px 3.5px 0px 3.5px;"
                     ) blank
