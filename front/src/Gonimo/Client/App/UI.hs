{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.App.UI where

import           Control.Lens
import qualified Data.Set                       as Set
import qualified GHCJS.DOM                      as DOM
import qualified GHCJS.DOM.Window               as Window
import qualified Language.Javascript.JSaddle    as JS
import           Reflex.Dom.Core

import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import           Gonimo.Client.App.Internal
import           Gonimo.Client.App.Types
import           Gonimo.Client.App.UI.I18N
import qualified Gonimo.Client.Auth             as Auth
import qualified Gonimo.Client.Baby             as Baby
import qualified Gonimo.Client.DeviceList       as DeviceList
import qualified Gonimo.Client.Family           as Family
import qualified Gonimo.Client.MessageBox       as MessageBox
import qualified Gonimo.Client.Parent           as Parent
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server           hiding (Config, Model, HasModel)
import qualified Gonimo.Client.Subscriber       as Subscriber
import qualified Gonimo.Client.Storage          as GStorage
import qualified Gonimo.Client.Storage.Keys     as GStorage
import           Gonimo.Client.Util             (getBrowserProperty,
                                                 getBrowserVersion)
import qualified Gonimo.SocketAPI               as API
import           Gonimo.Client.Settings         (HasConfig(onSelectLocale))


ui :: forall m t. GonimoM Model t m => m (ModelConfig t)
ui = mdo
  model <- ask
  checkBrowser
  family <- Family.family
            $ Family.fromApp model
              & Family.configSelectFamily .~ leftmost [ msgSwitchFamily
                                                      , familyUI^.Family.uiSelectFamily
                                                      ]
              & Family.configCreateFamily .~ familyUI^.Family.uiCreateFamily
              & Family.configLeaveFamily  .~ familyUI^.Family.uiLeaveFamily
              & Family.configSetName      .~ familyUI^.Family.uiSetName

  -- navEv <- navBar family
  -- let navCreateFamily = push (pure . (^?_Left)) navEv
  -- let navSelectFamily = push (pure . (^?_Right)) navEv

  msgBox <- MessageBox.ui $ MessageBox.fromApp model

  let msgSwitchFamily = push (pure . \case
                                [MessageBox.SelectFamily fid] -> Just fid
                                _ -> Nothing -- Dirty: We ignore selectfamily if multiple events occurred ...
                             ) (msgBox ^. MessageBox.action)

  accept <- AcceptInvitation.ui model
  (app, familyUI) <- runLoaded model family

  let oldServer = mempty & onRequest .~ ( app ^. request
                                          <> family ^. Family.request
                                        )
  let oldSubscriber = mempty & Subscriber.subscriptions .~ (family ^. Family.subscriptions <> app ^. subscriptions)
  let oldConfig = mempty
                  & serverConfig     .~ oldServer
                  & subscriberConfig .~ oldSubscriber
                  & settingsConfig . onSelectLocale .~ leftmost [ app^.selectLang
                                                                , familyUI^.Family.uiSelectLang
                                                                ]
  pure $ oldConfig <> accept

runLoaded :: forall model m t. (HasModel model t, GonimoM model t m)
      => Model t -> Family.Family t -> m (App t, Family.UI t)
runLoaded model family = do
  let mkFuncPair fa fb = (,) <$> fa <*> fb
  evReady <- waitForJust
             $ zipDynWith mkFuncPair (model^.Auth.authData) (family^.Family.families)
  familyGotCreated <- hold False $
                        push (pure . \case
                                API.ResCreatedFamily _ -> Just True
                                _ -> Nothing
                             ) (model^.onResponse)

  let onReady dynAuthFamilies =
        fromMaybeDyn
          (do
              Auth.connectionLossScreen model
              startUI <- Family.uiStart
              let subs = constDyn Set.empty
              pure (App subs never never, startUI)
          )
          (\selected -> do
              let loaded = Loaded (fst <$> dynAuthFamilies) (snd <$> dynAuthFamilies) selected
              familyCreated <- sample familyGotCreated
              loadedUI model loaded familyCreated
          )
          (family^.Family.selectedFamily)
  let notReady' = do
        elClass "div" "container" $ trText Loading_stay_tight
        pure never
  dynEvEv <- widgetHold notReady' (onReady <$> evReady)

  let evEv = switchPromptlyDyn dynEvEv -- Flatten Dynamic Event Event
  (,) <$> appSwitchPromptly (fst <$> evEv)
      <*> Family.uiSwitchPromptly (snd <$> evEv)


loadedUI :: forall model m t. (HasModel model t, GonimoM model t m)
      => Model t -> Loaded t -> Bool -> m (App t, Family.UI t)
loadedUI model loaded familyCreated = mdo
    deviceList <- DeviceList.deviceList
                    DeviceList.Config { DeviceList._configResponse = model^.onResponse
                                      , DeviceList._configAuthData = model^.Auth.authData
                                      , DeviceList._configFamilyId = loaded^.selectedFamily
                                      }
    initialRole <- getInitialRole
    dynPair <- widgetHold
              (renderCenter deviceList familyCreated initialRole)
              (renderCenter deviceList False <$> roleSelected)

    let screen = screenSwitchPromptlyDyn . fmap fst $ dynPair
    let familyUI = Family.uiSwitchPromptlyDyn . fmap snd $ dynPair

    let roleSelected = leftmost [ Just <$> familyUI^.Family.uiRoleSelected
                                , Nothing <$ screen^.screenGoHome
                                ]

    let app' = App { _request       = deviceList^.DeviceList.request
                                   <> screen^.screenApp.request
                                   <> familyUI^.Family.uiRequest
                   , _subscriptions = deviceList^.DeviceList.subscriptions
                                   <> screen^.screenApp.subscriptions
                   , _selectLang    = never
                   }
    pure (app', familyUI)
  where
    renderCenter :: DeviceList.DeviceList t -> Bool -> Maybe Family.GonimoRole -> m (Screen t, Family.UI t)
    renderCenter deviceList familyCreated' mRole =
      case mRole of
          Nothing -> do
            Auth.connectionLossScreen model
            (def,) <$> Family.ui model loaded familyCreated'
          Just Family.RoleBaby -> do
            Auth.connectionLossScreen model
            (, def) <$> Baby.ui model loaded deviceList
          -- Parent renders connection loss screen itself. (Should not be rendered when there is an alarm.)
          Just Family.RoleParent -> (,def) <$> Parent.ui model loaded deviceList

    getInitialRole = do
      isAutoStart <- Baby.readAutoStart
      pure $ if isAutoStart
             then Just Family.RoleBaby
             else Nothing


checkBrowser ::  forall model m t. GonimoM model t m => m ()
checkBrowser = do
    isiOS <- getBrowserProperty "ios"
    isBlink <- getBrowserProperty "blink"
    isChrome <- (||) <$> getBrowserProperty "chrome" <*> getBrowserProperty "chromium"
    -- isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
    isFirefox <- getBrowserProperty "gecko"
    isAndroid <- getBrowserProperty "android"
    browserVersion <- getBrowserVersion
    let major :: Double -> Int
        major = floor
    let warnMessage | isiOS = Just $ do
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
                     | (isFirefox &&  browserVersion < 52.0) ||
                       (isChrome  && (browserVersion < 55.0  ||
                                     (isAndroid && major browserVersion == 61)
                                     )
                       ) = Just $ do
                             el "h1" $ trText Please_upgrade_your_browser
                             el "br" blank
                             el "h2" $ trText Gonimo_might_not_work_as_expected
                             el "br" blank
                             trText Gonimo_needs_some_cutting_edge_technology
                             el "br" blank
                             trText If_you_can't_upgrade
                     | not isBlink && not isFirefox
                       = Just $ do
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

                     | otherwise = Nothing
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
      displayIt <- holdDyn (displayWarning' msg) $ pure never <$ gotAnswer
      gotAnswer <- switchHoldPromptly never =<< dyn displayIt
      pure gotAnswer

    displayWarning' :: m () -> m (Event t ())
    displayWarning' msg =
      elClass "div" "fullScreenOverlay" $
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
