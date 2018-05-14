{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.App.UI where

import           Control.Lens
import qualified GHCJS.DOM                      as DOM
import qualified GHCJS.DOM.Window               as Window
import qualified Language.Javascript.JSaddle    as JS
import           Reflex.Dom.Core
import           Reflex.Network

import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import           Gonimo.Client.App.Types
import           Gonimo.Client.App.UI.I18N
import qualified Gonimo.Client.Auth.Impl        as Auth
import qualified Gonimo.Client.Baby             as Baby
import qualified Gonimo.Client.DeviceList       as DeviceList
import qualified Gonimo.Client.Family           as Family
import qualified Gonimo.Client.MessageBox       as MessageBox
import qualified Gonimo.Client.Parent           as Parent
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Router           (Route (..), onGoBack, onSetRoute)
import qualified Gonimo.Client.Router           as Router
import           Gonimo.Client.Server           hiding (Config, HasModel, Model,
                                                 config)
import qualified Gonimo.Client.Server           as Server
import           Gonimo.Client.Settings         (HasConfig (onSelectLocale),
                                                 HasSettings, Settings)
import qualified Gonimo.Client.Storage          as GStorage
import qualified Gonimo.Client.Storage.Keys     as GStorage
import qualified Gonimo.Client.Subscriber.Impl  as Subscriber
import           Gonimo.Client.Util             (getBrowserProperty,
                                                 getBrowserVersion)
import qualified Gonimo.SocketAPI               as API
import           Gonimo.Client.ConfirmationButton (addConfirmation', _Yes, _No)

ui :: forall m t. GonimoM Model t m => m (ModelConfig t)
ui = mdo
  model <- ask
  checkBrowser
  family <- Family.family
            $ (Family.fromApp model) & Family.configSelectFamily .~ leftmost [ msgSwitchFamily
                                                                              , familyUI^.Family.uiSelectFamily
                                                                              ]
                                      & Family.configCreateFamily .~ familyUI^.Family.uiCreateFamily
                                      & Family.configLeaveFamily .~ familyUI^.Family.uiLeaveFamily
                                      & Family.configSetName .~ familyUI^.Family.uiSetName

  msgBox <- MessageBox.ui $ MessageBox.fromApp model

  leaveConf <- askLeaveConfirmation

  let msgSwitchFamily = push (\actions -> case actions of
                                [MessageBox.SelectFamily fid] -> pure $ Just fid
                                _ -> pure Nothing -- Dirty: We ignore selectfamily if multiple events occurred ...
                             ) (msgBox ^. MessageBox.action)

  accept <- AcceptInvitation.ui model
  (loadedConf, familyUI) <- runLoaded model family

  let withFamConf = loadedConf
                    & Server.onRequest %~ (family ^. Family.request <>)
                    & Subscriber.subscriptions %~ (family ^. Family.subscriptions <>)
                    & settingsConfig . onSelectLocale %~ (familyUI ^. Family.uiSelectLang <>)
  pure $ withFamConf <> accept <> leaveConf

-- | Show confirmation dialog on history back navigation.
--
--   If confirmed, another history back event gets fired - finally leaving the app.
--   If not confirmed, push another `RouteHome` on the stack so we can ask again
--   for confirmation next time.

--   TODO: This is copied from Parent.UI and adapted. We should provide a proper abstraction!
askLeaveConfirmation :: forall m t. (GonimoM Model t m) => m (ModelConfig t)
askLeaveConfirmation = do
    (onAddRoute, triggerAddRoute) <- newTriggerEvent
    -- Add another page to the history stack, so we can ask for confirmation.
    liftIO $ triggerAddRoute RouteHome

    route' <- view Router.route
    onWantsLeave <- everySecond -- Ignore building up events (onSetRoute below) ..
      $ push -- About to leave
          (\newRoute -> do
              currentRoute <- sample $ current route'
              if currentRoute == RouteHome && newRoute == RouteHome
                then pure $ Just ()
                else pure $ Nothing
          )
          (updated route')
    confirmed <- addConfirmation' leaveConfirmation onWantsLeave

    let
      onLeaveReq = fmapMaybe (preview _Yes) confirmed
      onStay     = fmapMaybe (fmap (const RouteHome) . preview _No) confirmed

    pure $ mempty & onSetRoute .~ leftmost [ onStay, onAddRoute ]
                  & onGoBack   .~ onLeaveReq

-- | Ask leave confirmation text for askLeaveConfirmation.
leaveConfirmation :: forall model m t. GonimoM model t m => m ()
leaveConfirmation = do
    el "h3" $ trText Really_stop_gonimo
    el "p"  $ trText Really_leave_the_app

runLoaded :: forall model m t. (HasModel model t, GonimoM model t m)
      => Model t -> Family.Family t -> m (ModelConfig t, Family.UI t)
runLoaded model family = do
  evReady <- waitForJust
             $ zipDynWith (liftM2 (,)) (model^.Auth.authData) (family^.Family.families)
  familyGotCreated <- hold False $ leftmost [ push (\res ->
                                                       case res of
                                                         API.ResCreatedFamily _ -> pure $ Just True
                                                         _ -> pure Nothing
                                                   ) (model^.onResponse)
                                            , False <$ ffilter isNothing (updated (family ^. Family.selectedFamily))
                                            ]

  let onReady dynAuthFamilies =
        fromMaybeDyn
          (do
              Auth.connectionLossScreen model
              startUI <- Family.uiStart
              pure (mempty, startUI)
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

  readyDyn <- holdDyn notReady' $ onReady <$> evReady
  evEv <- switchHold never =<< networkView readyDyn

  (,) <$> flatten (fst <$> evEv)
      <*> Family.uiSwitchPromptly (snd <$> evEv)


loadedUI :: forall model m t. (HasModel model t, GonimoM model t m)
      => Model t -> Loaded t -> Bool -> m (ModelConfig t, Family.UI t)
loadedUI model loaded familyCreated = mdo
    deviceList <- DeviceList.deviceList $ DeviceList.Config { DeviceList._configResponse = model^.onResponse
                                                            , DeviceList._configAuthData = model^.Auth.authData
                                                            , DeviceList._configFamilyId = loaded^.selectedFamily
                                                            }
    autoStartConf <- handleAutoStart

    -- We use double routes for being able to ask the user for confirmation before leave, thus
    -- we need to filter out multiple identical routes:
    uniqRoute <- holdUniqDyn $ model ^. Router.route

    -- Ugly hack, but I don't care for now as this code will get replaced soon anyway.
    familyCreatedBeh <- hold familyCreated $ False <$ updated uniqRoute

    evPair <- networkView $ renderCenter deviceList familyCreatedBeh <$> uniqRoute

    centerConf <- flatten . fmap fst $ evPair
    familyUI <- Family.uiSwitchPromptly . fmap snd $ evPair

    let baseConf = mempty & Server.onRequest .~ (  (deviceList ^. DeviceList.request)
                                                <> (familyUI ^. Family.uiRequest)
                                                )
                          & Subscriber.subscriptions .~ deviceList ^. DeviceList.subscriptions
                          & Router.onSetRoute .~ familyUI ^. Family.uiRouteSelected

    let fullConf = baseConf <> centerConf <> autoStartConf

    pure (fullConf, familyUI)
  where
    renderCenter :: DeviceList.DeviceList t -> Behavior t Bool -> Route -> m (ModelConfig t, Family.UI t)
    renderCenter deviceList familyCreated' route =
      case route of
          RouteHome -> do
            Auth.connectionLossScreen model
            (def,) <$> Family.ui model loaded familyCreated'
          RouteBaby -> do
            Auth.connectionLossScreen model
            (, def) <$> Baby.ui model loaded deviceList
          -- Parent renders connection loss screen itself. (Should not be rendered when there is an alarm.)
          RouteParent -> (, def) <$> Parent.ui loaded deviceList

    -- An event that triggers once, if autostart is enabled.
    -- handleAutoStart :: forall mConf. (IsConfig mConf, Router.HasConfig mConf) => m (mConf t)
    handleAutoStart = do
      isAutoStart <- Baby.readAutoStart
      if isAutoStart
      then do
        (ev, trigger) <- newTriggerEvent
        liftIO $ trigger RouteBaby
        pure $ mempty & Router.onSetRoute .~ ev
      else
        pure mempty


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
                      else if (isFirefox &&  browserVersion < 52.0) ||
                              (isChrome  && ( browserVersion < 55.0  ||
                                              (isAndroid && major browserVersion == 61)
                                            )
                              )
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
