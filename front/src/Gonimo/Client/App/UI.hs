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
import qualified GHCJS.DOM.Window                  as Window
import qualified Language.Javascript.JSaddle                       as JS
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Util (getBrowserProperty, getBrowserVersion)


ui :: forall m t. GonimoM t m
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


runLoaded :: forall m t. GonimoM t m
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
              Auth.connectionLossScreen $ config^.auth
              startUI <- Family.uiStart
              let subs = constDyn Set.empty
              pure (App subs never never, startUI)
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


loadedUI :: forall m t. GonimoM t m
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
                  , _selectLang = never
                  }
    pure (app, familyUI)
  where
    renderCenter :: DeviceList.DeviceList t -> Bool -> Maybe Family.GonimoRole -> m (Screen t, Family.UI t)
    renderCenter deviceList familyCreated' mRole =
      case mRole of
          Nothing -> do
            Auth.connectionLossScreen $ config^.auth
            (def,) <$> Family.ui config loaded familyCreated'
          Just Family.RoleBaby -> do
            Auth.connectionLossScreen $ config^.auth
            (, def) <$> Baby.ui config loaded deviceList
          -- Parent renders connection loss screen itself. (Should not be rendered when there is an alarm.)
          Just Family.RoleParent -> (,def) <$> Parent.ui config loaded deviceList

    getInitialRole = do
      isAutoStart <- Baby.readAutoStart
      pure $ if isAutoStart
             then Just Family.RoleBaby
             else Nothing


checkBrowser ::forall m t. GonimoM t m => m ()
checkBrowser = do
    isiOS <- getBrowserProperty "ios"
    isBlink <- getBrowserProperty "blink"
    isChrome <- (||) <$> getBrowserProperty "chrome" <*> getBrowserProperty "chromium"
    -- isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
    isFirefox <- getBrowserProperty "gecko"
    browserVersion <- getBrowserVersion
    let warnMessage = if isiOS
                      then Just $ do
                        el "h1" $ text "We are sorry, Apple does not like us yet!"
                        el "br" blank
                        el "h2" $ text "Gonimo might not work as expected"
                        el "br" blank
                        text "Unfortunately, Apple iOS devices cannot be supported right now, because Safari does not implement the necessary technology."
                        el "br" blank
                        text "Also Apple restricts all other browsers on iOS to the same technology as Safari, so on iOS not even Chrome will work!"
                        el "br" blank
                        text "Fortunately Apple has made some progress lately and it seems that Safari will support Gonimo soon! You can follow us on "
                        elAttr "a" ("class" =: "link" <> "href" =: "https://facebook.com/mygonimo")
                          $ text "Facebook"
                        text ": We will post on our page, when iOS support is ready!"
                      else if (isFirefox && browserVersion < 52.0)
                              || (isChrome && browserVersion < 55.0)
                           then  Just $ do
                             el "h1" $ text "Please upgrade your browser!"
                             el "br" blank
                             el "h2" $ text "Gonimo might not work as expected!"
                             el "br" blank
                             text "Gonimo needs some bleeding edge technology in order to work correctly and browsers get better all the time, so we recommend to download the latest version of your browsers, for the best gonimo experience."
                             el "br" blank
                             text " If you can't upgrade, please double check that gonimo is working reliably for you. Especially check that you will hear an alarm whenever the connection is lost (you can test this by, for example, reloading the page at your baby station), especially check that you also hear an alarm when the screen is switched off at the parent station."
                      else if not isBlink && not isFirefox
                           then Just $ do
                            el "h1" $ text "Unsupported browser!"
                            el "br" blank
                            el "h2" $ text "Gonimo might not work as expected"
                            el "br" blank
                            text "We either never tested gonimo with your browser or it is not supported right now. Please be aware that gonimo might not work properly! If you want the best experience we currently have to recommend Chrome and any non iOS platform. If you'd like to have your browser supported or want to know about the current status, please file an issue on "
                            elAttr "a" ("class" =: "link" <> "href" =: "https://github.com/gonimo/gonimo/issues")
                              $ text "github!"
                            el "br" blank
                            text "Thank you!"

                           else Nothing
    case warnMessage of
      Nothing -> pure ()
      Just msg -> do
        _ <- displayWarning msg
        pure ()

    -- Warn everytime - so user will know when the browser gets supported!
    -- performEvent_ $ const (writeHideBrowserWarning True) <$> okClicked
  where
    displayWarning :: m () -> m (Event t ())
    displayWarning msg = mdo
      displayIt <- holdDyn (displayWarning' msg) $ const (pure never) <$> gotAnswer
      gotAnswer <- switchPromptly never =<< dyn displayIt
      pure gotAnswer

    displayWarning' :: m () -> m (Event t ())
    displayWarning' msg = do
      elClass "div" "fullScreenOverlay" $ do
        elClass "div" "container" $ do
          msg
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
