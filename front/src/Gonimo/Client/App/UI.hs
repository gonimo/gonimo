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
import qualified Gonimo.SocketAPI               as API
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified Language.Javascript.JSaddle                       as JS
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Util (getBrowserProperty)


ui :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> m (App t)
ui config = mdo
  checkBrowser
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
             & subscriptions %~ (<> accept^.AcceptInvitation.subscriptions)


runLoaded :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> Family.Family t -> m (App t, Family.UI t)
runLoaded config family = do
  let mkFuncPair fa fb = (,) <$> fa <*> fb
  evReady <- waitForJust
             $ zipDynWith mkFuncPair (config^.auth.Auth.authData) (family^.Family.families)
  familyGotCreated <- hold False $ push (\res ->
                                            case res of
                                              API.ResCreatedFamily _ -> pure $ Just True
                                              _ -> pure Nothing
                                        ) (config^.server.webSocket_recv)

  let onReady dynAuthFamilies =
        fromMaybeDyn
          (do
              startUI <- Family.uiStart
              let subs = constDyn Set.empty
              pure (App subs never, startUI)
          )
          (\selected -> do
              let loaded = Loaded (fst <$> dynAuthFamilies) (snd <$> dynAuthFamilies) selected
              familyCreated <- sample familyGotCreated
              loadedUI config loaded familyCreated
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
      => Config t -> Loaded t -> Bool -> m (App t, Family.UI t)
loadedUI config loaded familyCreated = mdo
    deviceList <- DeviceList.deviceList $ DeviceList.Config { DeviceList._configResponse = config^.server.webSocket_recv
                                                            , DeviceList._configAuthData = config^.auth.Auth.authData
                                                            , DeviceList._configFamilyId = loaded^.selectedFamily
                                                            }
    initialRole <- getInitialRole
    dynPair <- widgetHold
              (renderCenter deviceList familyCreated initialRole)
              (renderCenter deviceList False <$> roleSelected)

    let screen = screenSwitchPromptlyDyn . fmap fst $ dynPair
    let familyUI = Family.uiSwitchPromptlyDyn . fmap snd $ dynPair

    let roleSelected = leftmost [ Just <$> familyUI^.Family.uiRoleSelected
                                , const Nothing <$> screen^.screenGoHome
                                ]

    let app = App { _request = deviceList^.DeviceList.request <> screen^.screenApp.request
                              <> familyUI^.Family.uiRequest
                  , _subscriptions = deviceList^.DeviceList.subscriptions <> screen^.screenApp.subscriptions
                  }
    pure (app, familyUI)
  where
    renderCenter :: DeviceList.DeviceList t -> Bool -> Maybe Family.GonimoRole -> m (Screen t, Family.UI t)
    renderCenter deviceList familyCreated' mRole =
      case mRole of
          Nothing -> (def,) <$> Family.ui config loaded familyCreated'
          Just Family.RoleBaby -> (, def) <$> Baby.ui config loaded deviceList
          Just Family.RoleParent -> (,def) <$> Parent.ui config loaded deviceList

    getInitialRole = do
      isAutoStart <- Baby.readAutoStart
      pure $ if isAutoStart
             then Just Family.RoleBaby
             else Nothing


checkBrowser ::forall m t. (HasWebView m, MonadWidget t m) => m ()
checkBrowser = do
    isiOS <- getBrowserProperty "ios"
    isBlink <- getBrowserProperty "blink"
    -- hideWarning <- readHideBrowserWarning
    let hideWarning = False

    let warnMessage = if isiOS
                      then "Unfortunately Apple iOS devices cannot be supported right now, because Safari does not implement the necessary technology. Also Apple restricts all other browsers on iOS to the same technology Safari supports, so on iOS not even Chrome will work."
                      else if not isBlink
                           then "Unfortunately the only fully supported browser currently is Chrome. This is especially true on mobile, here even if Firefox works for you - beware that the connection-loss alert will not be played when your device's screen is switched off!"
                           else "All fine!"
    let warningRequired = not hideWarning && (isiOS || not isBlink)
    if warningRequired
      then do
      _ <- displayWarning warnMessage
      pure ()
      -- Warn everytime - so user will know when the browser gets supported!
      -- performEvent_ $ const (writeHideBrowserWarning True) <$> okClicked
      else
      pure ()
  where
    displayWarning msg = mdo
      displayIt <- holdDyn (displayWarning' msg) $ const (pure never) <$> gotAnswer
      gotAnswer <- switchPromptly never =<< dyn displayIt
      pure gotAnswer

    displayWarning' msg = do
      elClass "div" "fullScreenOverlay" $ do
        elClass "div" "container" $ do
          el "h1" $ text "Unsupported browser or device!"
          el "br" blank
          el "h2" $ text "Gonimo might not work as expected"
          el "br" blank
          text msg
          el "br" blank
          el "br" blank
          makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "OK"
    

readHideBrowserWarning :: JS.MonadJSM m => m Bool
readHideBrowserWarning = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  fromMaybe False <$> GStorage.getItem storage GStorage.hideBrowserWarning

writeHideBrowserWarning :: JS.MonadJSM m => Bool -> m ()
writeHideBrowserWarning hideBrowserWarning = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.hideBrowserWarning hideBrowserWarning
