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

import qualified GHCJS.DOM                      as DOM
import qualified GHCJS.DOM.Window               as Window
import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import           Gonimo.Client.App.Internal
import           Gonimo.Client.App.Types
import           Gonimo.Client.App.UI.I18N
import qualified Gonimo.Client.Auth             as Auth
import qualified Gonimo.Client.Baby             as Baby
import qualified Gonimo.Client.Family           as Family
import qualified Gonimo.Client.MessageBox       as MessageBox
import qualified Gonimo.Client.Parent           as Parent
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server hiding (Config, config)
import qualified Gonimo.Client.Storage          as GStorage
import qualified Gonimo.Client.Storage.Keys     as GStorage
import           Gonimo.Client.Util             (getBrowserProperty,
                                                 getBrowserVersion)
import qualified Gonimo.SocketAPI               as API
import qualified Language.Javascript.JSaddle    as JS


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
             & selectLang .~ leftmost [app^.selectLang, familyUI^.Family.uiSelectLang ]


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
                                        ) (config^.server.response)

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
  let notReady' = do
        elClass "div" "container" $ trText Loading_stay_tight
        pure never
  dynEvEv <- widgetHold notReady' (onReady <$> evReady)

  let evEv = switchPromptlyDyn dynEvEv -- Flatten Dynamic Event Event
  (,) <$> appSwitchPromptly (fst <$> evEv)
      <*> Family.uiSwitchPromptly (snd <$> evEv)


loadedUI :: forall m t. GonimoM t m
      => Config t -> Loaded t -> Bool -> m (App t, Family.UI t)
loadedUI config loaded familyCreated = mdo
    deviceList <- DeviceList.deviceList $ DeviceList.Config { DeviceList._configResponse = config^.server.response
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
                        el "h1" $ trText IOS_support_is_in_the_works
                        el "br" blank
                        -- el "h2" $ trText Gonimo_might_not_work_as_expected
                        -- el "br" blank
                        trText We_are_sorry_right_now_iOS_devices_might_not_work_as_expected
                        el "br" blank
                        trText Once_we_verified_that_everything_works_as_expected_this_message_will_disappear
                        elAttr "a" ("class" =: "link" <> "href" =: "https://facebook.com/mygonimo")
                          $ text "Facebook"
                        trText Or_On_Our
                        elAttr "a" ("class" =: "link" <> "href" =: "https://blog.gonimo.com")
                          $ text "blog"
                        trText To_stay_up_to_date_on_the_progress
                      else if (isFirefox && browserVersion < 52.0)
                              || (isChrome && browserVersion < 55.0)
                           then  Just $ do
                             el "h1" $ trText Please_upgrade_your_browser
                             el "br" blank
                             el "h2" $ trText Gonimo_might_not_work_as_expected
                             el "br" blank
                             trText Gonimo_needs_some_cutting_edge_technology
                             el "br" blank
                             trText If_you_can't_upgrade
                      else if not isBlink && not isFirefox
                           then Just $ do
                            el "h1" $ trText Unsupported_browser
                            el "br" blank
                            el "h2" $ trText Gonimo_might_not_work_as_expected
                            el "br" blank
                            trText We_either_never_tested_gonimo_with_your_browser
                            elAttr "a" ("class" =: "link" <> "href" =: "https://github.com/gonimo/gonimo/issues")
                              $ text "github!"
                            el "br" blank
                            trText Thank_you
                            text "!"

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
          makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText OK
    

readHideBrowserWarning :: JS.MonadJSM m => m Bool
readHideBrowserWarning = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  fromMaybe False <$> GStorage.getItem storage GStorage.hideBrowserWarning

writeHideBrowserWarning :: JS.MonadJSM m => Bool -> m ()
writeHideBrowserWarning hideBrowserWarning = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.hideBrowserWarning hideBrowserWarning
