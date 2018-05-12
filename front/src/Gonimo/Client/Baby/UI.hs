{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text.IO                     as T

import           GHCJS.DOM.Types                  (MediaStream)
import qualified GHCJS.DOM.Types                  as JS
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types          as App
import           Gonimo.Client.Baby.Internal
import qualified Gonimo.Client.Baby.Socket        as Socket
import           Gonimo.Client.Baby.UI.I18N
import           Gonimo.Client.ConfirmationButton (mayAddConfirmation,
                                                   mayAddConfirmation', _No,
                                                   _Yes)
import qualified Gonimo.Client.DeviceList         as DeviceList
import           Gonimo.Client.EditStringButton   (editStringEl)
import qualified Gonimo.Client.NavBar             as NavBar
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Router             (HasRouter (..), Route (..),
                                                   onGoBack, onSetRoute)
import qualified Gonimo.Client.Router             as Router
import           Gonimo.Client.Server             hiding (Config, HasModel)
import qualified Gonimo.Client.Server             as Server
import qualified Gonimo.Client.Subscriber         as Subscriber
import qualified Gonimo.Client.Host         as Host
import           Gonimo.Client.Util
import           Gonimo.Client.Reflex



data BabyScreen = ScreenStart | ScreenRunning

ui :: forall model mConf m t
  . (HasModel model, GonimoM model t m, HasModelConfig mConf t)
  => App.Model t -> App.Loaded t -> DeviceList.DeviceList t -> m (mConf t)
ui appConfig loaded deviceList = mdo
    baby' <- baby $ Config { _configNextCamera = ui'^.uiSelectCamera
                           , _configEnableCamera = ui'^.uiEnableCamera
                           , _configEnableAutoStart = leftmost [ui'^.uiEnableAutoStart, disableAutostart]
                           , _configResponse = appConfig^.onResponse
                           , _configAuthData = loaded^.App.authData
                           , _configStartMonitor = startMonitor
                           , _configStopMonitor  = stopMonitor
                           , _configSetBabyName = ui'^.uiSetBabyName
                           , _configSelectedFamily = loaded^.App.selectedFamily
                           , _configGetUserMedia = errorNewStream
                           }

    (autoStartEv, triggerAutoStart) <- newTriggerEvent
    doAutoStart <- readAutoStart

    uiDyn <- widgetHold (uiStart loaded deviceList baby') (renderCenter baby' <$> screenSelected)

    disableAutostart <-
      if doAutoStart
      then do
        disabled <- autoStartActiveMessage
        liftIO $ triggerAutoStart ()
        pure $ const False <$> disabled
      else
        pure never

    let startMonitor = leftmost [ ui'^.uiStartMonitor
                                , autoStartEv
                                ]
    let stopMonitor = leftmost [ onCleanupRequested
                               , ui'^.uiStopMonitor
                               ]
    killMask <- hold True $ leftmost [ False <$ startMonitor
                                     , True <$ stopMonitor
                                     ]
    let ui' = uiSwitchPromptlyDyn uiDyn

    (onCleanupRequested, leaveConf) <- askLeaveConfirmation baby'

    errorNewStreamEv <-
      dyn $ showPermissionError <$> baby'^.mediaStream
    errorNewStream <- switchPromptly never $ snd <$> errorNewStreamEv
    errorGoHome <- switchPromptly never $ fst <$> errorNewStreamEv

    let screenSelected = leftmost [ const ScreenStart <$> ui'^.uiStopMonitor
                                  , const ScreenRunning <$> startMonitor
                                  ]

    performEvent_ $ const (do
                              cStream <- sample $ current (baby'^.mediaStream)
                              traverse_ stopMediaStream cStream
                          ) <$> onCleanupRequested

    let mConf = mempty & Subscriber.subscriptions .~ baby'^.socket.Socket.subscriptions
                       & Server.onRequest .~ mconcat [ baby'^.socket.Socket.request
                                                     , baby'^.request
                                                     , ui'^.uiRequest
                                                     ]
                       & Router.onGoBack .~  leftmost [ ui'^.uiGoHome
                                                      , errorGoHome
                                                      ]
                       & Host.appKillMask .~ killMask
    pure $ mconcat [ leaveConf
                   , mConf
                   ]
  where
    renderCenter baby' ScreenStart   = uiStart loaded deviceList baby'
    renderCenter baby' ScreenRunning = uiRunning loaded deviceList baby'

-- | Show confirmation dialog on history back navigation.
--
--   If confirmed, another history back event gets fired - finally leaving the parent station.
--   If not confirmed, push another `RouteParent` on the stack so we can ask again
--   for confirmation next time.

--   The returned event fires, if the user confirmed leaving, but before any
--   cleanup happened. Thus it can and must be used for triggering the needed
--   cleanup.
--
--   TODO: If the user presses the back button twice - we don't do any cleanup!
--   Any baby connections will stay alive!
--
--   TODO: This is copied from Parent.UI and adapted. We should provide a proper abstraction!
askLeaveConfirmation :: forall model mConf m t.
  ( GonimoM model t m, HasRouter model
  , HasModelConfig mConf t
  )
  => Baby t -> m (Event t (), mConf t)
askLeaveConfirmation baby' = do
  (onAddRoute, triggerAddRoute) <- newTriggerEvent
  -- Add another page to the history stack, so we can ask for confirmation.
  liftIO $ triggerAddRoute RouteBaby

  let needConfirmation = not . Map.null <$> baby'^.socket^.Socket.channels

  route' <- view Router.route
  onWantsLeave <- everySecond
    $ push
        (\newRoute -> do
            currentRoute <- sample $ current route'
            if currentRoute == RouteBaby  && newRoute == RouteBaby
            then pure $ Just ()
            else pure $ Nothing
        )
        (updated route')
  confirmed <- mayAddConfirmation' leaveConfirmation onWantsLeave needConfirmation

  let
    onLeaveReq = fmapMaybe (preview _Yes) confirmed
    onStay     = fmapMaybe (fmap (const RouteBaby) . preview _No) confirmed

  let conf = mempty & onSetRoute .~ leftmost [ onStay, onAddRoute ]
                    & onGoBack   .~ onLeaveReq
  pure (onLeaveReq, conf)


uiStart :: forall model m t. GonimoM model t m => App.Loaded t -> DeviceList.DeviceList t -> Baby t
            -> m (UI t)
uiStart loaded deviceList  baby' = do
    elClass "div" "container" $ do
      elClass "div" "baby" $ mdo
        navBar <- NavBar.navBar (NavBar.Config loaded deviceList)

        _ <- widgetHold (pure ()) $ showAutostartInfo <$> updated (baby'^.autoStartEnabled)

        newBabyName <-
          setBabyNameForm loaded baby'
        _ <- dyn $ renderVideo <$> baby'^.mediaStream
        startClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText Start
        renderVolumemeter $ baby'^.volumeLevel
        elClass "div" "stream-menu" $ do
          selectCamera <- cameraSelect baby'
          autoStart <- enableAutoStartCheckbox baby'
          enableCamera <- enableCameraCheckbox baby'
          pure $ UI { _uiGoHome = leftmost [ navBar^.NavBar.homeClicked, navBar^.NavBar.backClicked ]
                    , _uiStartMonitor = startClicked
                    , _uiStopMonitor = never -- already there
                    , _uiEnableCamera = enableCamera
                    , _uiEnableAutoStart = autoStart
                    , _uiSelectCamera = selectCamera
                    , _uiSetBabyName = newBabyName
                    , _uiRequest = navBar^.NavBar.request
                    }
  where
    renderVideo (Left _) = pure ()
    renderVideo (Right stream)
      = elClass "div" "stream-baby" $
        mediaVideo stream ( "autoplay" =: "true"
                          <> "muted" =: "true"
                          )
    showAutostartInfo False = pure ()
    showAutostartInfo True = dismissibleOverlay "info-overlay" 7
                             $ trText Baby_monitor_will_start_automatically

uiRunning :: forall model m t. GonimoM model t m => App.Loaded t -> DeviceList.DeviceList t -> Baby t -> m (UI t)
uiRunning loaded deviceList baby' =
  elClass "div" "container" $ mdo
    displayScreenOnWarning baby'
    autoStartOn <- sample $ current (baby'^.autoStartEnabled)
    let dayNightStart = if autoStartOn then "night" else "day"
    dayNight <- holdDyn dayNightStart $ tag toggledDayNight dayNightClicked
    let
      toggledDayNight :: Behavior t Text
      toggledDayNight = (\c -> if c == "day" then "night" else "day") <$> current dayNight
    let
      babyClass :: Dynamic t Text
      babyClass = pure "baby setup-done " <> dayNight

    (ui', dayNightClicked) <-
      elDynClass "div" babyClass $ do
        elClass "div" "good-night" $ do
          el "h1" $ trText Good_Night
          el "h2" $ dynText $ baby'^.name <> pure "!"
        elClass "div" "fill-full-screen" blank
        _ <- dyn $ noSleep <$> baby'^.mediaStream

        navBar' <- NavBar.navBar (NavBar.Config loaded deviceList)

        let needConfirmation = not . Map.null <$> baby'^.socket^.Socket.channels
        navBar <- NavBar.NavBar
                  <$> mayAddConfirmation leaveConfirmation (navBar'^.NavBar.backClicked) needConfirmation
                  <*> pure (navBar' ^. NavBar.homeClicked)
                  <*> pure (navBar' ^. NavBar.request)

        dayNightClicked' <- makeClickable . elAttr' "div" (addBtnAttrs "time") $ blank
        stopClicked <- elClass "div" "stream-menu" $
          flip (mayAddConfirmation leaveConfirmation) needConfirmation
          =<< (makeClickable . elAttr' "div" (addBtnAttrs "stop") $ trText Stop)
        -- stopClicked <- flip (mayAddConfirmation leaveConfirmation) needConfirmation
        --               =<< (makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText Stop)
        let handleStop f = push (\_ -> do
                                  autoStartOn' <- sample $ current (baby'^.autoStartEnabled)
                                  if f autoStartOn'
                                  then pure $ Nothing
                                  else pure $ Just ()
                                ) stopClicked
        let stopGoHome = handleStop id
        let stopGoBack = handleStop not

        let ui'' = UI { _uiGoHome = leftmost [navBar^.NavBar.homeClicked, stopGoHome]
                      , _uiStartMonitor = never
                      , _uiStopMonitor = leftmost [navBar^.NavBar.backClicked, stopGoBack]
                      , _uiEnableCamera = never
                      , _uiEnableAutoStart = never
                      , _uiSelectCamera = never
                      , _uiSetBabyName = never
                      , _uiRequest = navBar^.NavBar.request
                      }
        pure (ui'', dayNightClicked')
    pure ui'
  where
    noSleep (Left _) = pure ()
    noSleep (Right stream)
      = mediaVideo stream ( "style" =: "display:none"
                            <> "autoplay" =: "true"
                            <> "muted" =: "true"
                          )

leaveConfirmation :: forall model m t. GonimoM model t m => m ()
leaveConfirmation = do
    el "h3" $ trText Really_stop_baby_monitor
    el "p" $ trText All_connected_devices_will_be_disconnected

cameraSelect :: forall model m t. GonimoM model t m => Baby t -> m (Event t ())
cameraSelect baby' =
  if baby'^.cameraCount > 1
  then makeClickable . elAttr' "div" (addBtnAttrs "cam-switch") $ el "span" blank
  else pure never

enableCameraCheckbox :: forall model m t. GonimoM model t m
                => Baby t -> m (Event t Bool)
enableCameraCheckbox baby' =
  if baby'^.cameraCount > 0
  then myCheckBox  ("class" =: "cam-on ") (baby'^.cameraEnabled) $ text "\xf03d"
  else pure never -- No need to enable the camera when there is none!

enableAutoStartCheckbox :: forall model m t. GonimoM model t m
                => Baby t -> m (Event t Bool)
enableAutoStartCheckbox baby' =
    myCheckBox ("class" =: "autostart ") (baby'^.autoStartEnabled) $ trText Autostart

setBabyNameForm :: forall model m t. GonimoM model t m
                   => App.Loaded t -> Baby t -> m (Event t Text)
setBabyNameForm loaded baby' = do
  (nameAddRequest, selectedName) <-
    elClass "div" "welcome-form baby-form" $ mdo
      elClass "span" "baby-form" $ trText Adjust_camera_for
      elClass "span" "baby-text" $ trText BabyText

      clicked <-
        makeClickable . elAttr' "div" (addBtnAttrs "family-select") $ do
          dynText $ baby'^.name
          text " "
          elClass "span" "baby-caret fa fa-caret-down" blank

      nameAddRequest <-
        makeClickable $ elAttr' "div" (addBtnAttrs "input-btn plus baby-form") blank
      -- pure (clicked', nameAdded')

      let openClose = pushAlways (\_ -> not <$> sample (current droppedDown)) clicked
      droppedDown <- holdDyn False $ leftmost [ openClose
                                              , const False <$> selectedName
                                              ]
      let
        droppedDownClass :: Dynamic t Text
        droppedDownClass = fmap (\opened -> if opened then "isDroppedDown " else "") droppedDown
      let
        dropDownClass :: Dynamic t Text
        dropDownClass = pure "dropDown-container " <> droppedDownClass

      selectedName <-
        elDynClass "div" dropDownClass $ renderBabySelectors (App.babyNames loaded)
      pure (nameAddRequest, selectedName)
  -- Necessary for stacking order with volumemeter:
  nameAdded <- editStringEl (pure nameAddRequest) (trText Add_new_baby_name) (constDyn "")
  pure $ leftmost [ selectedName, nameAdded ]

renderBabySelectors :: forall model m t. GonimoM model t m
                    => Dynamic t [Text] -> m (Event t Text)
renderBabySelectors names =
  let
    renderBabySelector :: Text -> m (Event t Text)
    renderBabySelector name' = do
        fmap (fmap (const name')) . el "div" $ do
          makeClickable . elAttr' "a" (addBtnAttrs "") $ text name'

    renderSelectors names' =
      let
        names'' = case names' of
                    [] -> [""] -- So user gets feedback on click!
                    _  -> names'
      in
        leftmost <$> traverse renderBabySelector names''
  in
    elClass "div" "family-select" $
      switchPromptly never =<< (dyn $ renderSelectors <$> names)

displayScreenOnWarning :: forall model m t. GonimoM model t m
            => Baby t -> m ()
displayScreenOnWarning baby' = mdo
    isMobile <- getBrowserProperty "mobile"
    isTablet <- getBrowserProperty "tablet"
    let haveVideo = baby'^.cameraEnabled
    let needWarning = (&& (isMobile || isTablet)) <$> haveVideo

    _ <- dyn $ displayWarning <$> needWarning
    pure ()

  where
    displayWarning False = pure ()
    displayWarning True = dismissibleOverlay "info-overlay"  10 $ do
      trText For_video_to_work_please_do_not_switch_off_the_screen
      el "br" blank
      trText Alternatively_if_all_you_need_is_audio_please_disable_the_camera

autoStartActiveMessage :: forall model m t. GonimoM model t m => m (Event t ())
autoStartActiveMessage = do
  (disableEv, triggerDisable) <- newTriggerEvent
  dismissibleOverlay "info-overlay" 2 $ do
    trText Autostart_active
    el "br" blank
    el "br" blank
    clicked <- makeClickable . elAttr' "div" (addBtnAttrs "stop") $ trText Disable
    performEvent_ $ const (liftIO $ triggerDisable ())  <$> clicked
  pure disableEv


showPermissionError :: forall model m t. GonimoM model t m => Either JS.PromiseRejected MediaStream ->  m (Event t (), Event t ())
showPermissionError (Right _) = pure (never, never)
showPermissionError (Left (JS.PromiseRejected err)) = elClass "div" "fullScreenOverlay" $ do
    errText <- showJSException err
    liftIO $ T.putStrLn $ "gonimo HaskellActivity, getUserMedia failed:" <> errText
    backClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
    el "h1" $ trText Error_so_sad
    el "h2" $ trText Can_not_access_your_camera_or_microphone
    el "br" blank
    trText Obviously_those_are_needed_for_a_baby_monitor
    el "br" blank
    el "br" blank
    isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
    isChrome <- getBrowserProperty "blink"
    when isChrome $ do
      if isMobile
        then trText Please_click_on_the_lock_symbol
        else trText Please_click_on_the_lock_or_the_camera_symbol
    el "br" blank
    el "br" blank
    retry <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText Try_Again
    pure (backClicked, retry)

