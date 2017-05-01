{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Gonimo.Client.DeviceList          as DeviceList
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types           as App
import           Gonimo.Client.Baby.Internal
import qualified Gonimo.Client.Baby.Socket         as Socket
import qualified Gonimo.Client.NavBar              as NavBar
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server              (webSocket_recv)
import           Gonimo.DOM.Navigator.MediaDevices
import           Gonimo.Client.EditStringButton    (editStringEl)
import           Gonimo.Client.ConfirmationButton  (mayAddConfirmation)
import           Gonimo.Client.Util
import qualified Data.Map.Strict as Map
import           Data.Foldable
import           GHCJS.DOM.NavigatorUserMediaError (UserMediaException(..))
import           GHCJS.DOM.Types                   (MediaStream)
import           Control.Monad
import           Gonimo.Client.Baby.UI.I18N
import           Gonimo.I18N

data BabyScreen = ScreenStart | ScreenRunning

ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> m (App.Screen t)
ui appConfig loaded deviceList = mdo
    baby' <- baby $ Config { _configSelectCamera = ui'^.uiSelectCamera
                           , _configEnableCamera = ui'^.uiEnableCamera
                           , _configEnableAutoStart = leftmost [ui'^.uiEnableAutoStart, disableAutostart]
                           , _configResponse = appConfig^.App.server.webSocket_recv
                           , _configAuthData = loaded^.App.authData
                           , _configStartMonitor = startMonitor
                           , _configStopMonitor  = leftmost [ui'^.uiGoHome, ui'^.uiStopMonitor]
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

    let ui' = uiSwitchPromptlyDyn uiDyn

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
                          ) <$> ui'^.uiGoHome
    let babyApp = App.App { App._subscriptions = baby'^.socket.Socket.subscriptions
                          , App._request = baby'^.socket.Socket.request <> baby'^.request
                                           <> ui'^.uiRequest
                          }
    pure $ App.Screen { App._screenApp = babyApp
                      , App._screenGoHome = leftmost [ui'^.uiGoHome, errorGoHome]
                      }
  where
    renderCenter baby' ScreenStart = uiStart loaded deviceList baby'
    renderCenter baby' ScreenRunning = uiRunning loaded deviceList baby'

uiStart :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Loaded t -> DeviceList.DeviceList t -> Baby t
            -> m (UI t)
uiStart loaded deviceList  baby' = do
    elClass "div" "container" $ do
      elClass "div" "baby" $ mdo
        navBar <- NavBar.navBar (NavBar.Config loaded deviceList)

        _ <- widgetHold (pure ()) $ showAutostartInfo <$> updated (baby'^.autoStartEnabled)
  
        newBabyName <-
          setBabyNameForm loaded baby'
        _ <- dyn $ renderVideo <$> baby'^.mediaStream
        startClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text $ i18n EN Start
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
                             $ text "Baby monitor will start automatically, when you load the app next time."

uiRunning :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Loaded t -> DeviceList.DeviceList t -> Baby t -> m (UI t)
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
          el "h1" $ text $ i18n EN Good_Night
          el "h2" $ dynText $ baby'^.name <> pure "!"
        elClass "div" "fill-full-screen" blank
        _ <- dyn $ noSleep <$> baby'^.mediaStream
        let
          leaveConfirmation :: forall m1. (HasWebView m1, MonadWidget t m1) => m1 ()
          leaveConfirmation = do
              el "h3" $ text $ i18n EN Really_stop_baby_monitor
              el "p" $ text $ i18n EN All_connected_devices_will_be_disconnected 

        navBar' <- NavBar.navBar (NavBar.Config loaded deviceList)

        let needConfirmation = not . Map.null <$> baby'^.socket^.Socket.channels
        navBar <- NavBar.NavBar
                  <$> mayAddConfirmation leaveConfirmation (navBar'^.NavBar.backClicked) needConfirmation
                  <*> mayAddConfirmation leaveConfirmation (navBar'^.NavBar.homeClicked) needConfirmation
                  <*> pure (navBar'^.NavBar.request)

        dayNightClicked' <- makeClickable . elAttr' "div" (addBtnAttrs "time") $ blank
        stopClicked <- elClass "div" "stream-menu" $
          flip (mayAddConfirmation leaveConfirmation) needConfirmation
          =<< (makeClickable . elAttr' "div" (addBtnAttrs "stop") $ text $ i18n EN Stop)
        -- stopClicked <- flip (mayAddConfirmation leaveConfirmation) needConfirmation
        --               =<< (makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text $ i18n EN Stop)
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


cameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Text)
cameraSelect baby' = do
  evEv <- dyn $ cameraSelect' baby' <$> baby'^.videoDevices
  switchPromptly never evEv

cameraSelect' :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> [MediaDeviceInfo] -> m (Event t Text)
cameraSelect' baby' videoDevices' =
  case videoDevices' of
    [] -> pure never
    [_] -> pure never
    _   -> mdo
            clicked <-
              makeClickable . elAttr' "div" (addBtnAttrs "cam-switch") $ el "span" blank

            let openClose = pushAlways (\_ -> not <$> sample (current droppedDown)) clicked
            droppedDown <- holdDyn False $ leftmost [ openClose
                                                    , const False <$> selectedName
                                                    ]
            let
              droppedDownClass :: Dynamic t Text
              droppedDownClass = fmap (\opened -> if opened then "isDroppedDown " else "") droppedDown
            let
              dropDownClass :: Dynamic t Text
              dropDownClass = pure "dropUp-container " <> droppedDownClass

            selectedName <-
              elDynClass "div" dropDownClass $ renderCameraSelectors cameras
            pure selectedName
  where
    selectedCameraText = fromMaybe (i18n EN Standard_Setting) <$> baby'^.selectedCamera

    cameras = map mediaDeviceLabel videoDevices'

    renderCameraSelectors :: [Text] -> m (Event t Text)
    renderCameraSelectors cams =
      elClass "div" "family-select" $
        leftmost <$> traverse renderCameraSelector cams

    renderCameraSelector :: Text -> m (Event t Text)
    renderCameraSelector label = do
        clicked <-
          makeClickable
          . elAttr' "div" (addBtnAttrs "") $ do
              text label
              dynText $ ffor selectedCameraText (\selected -> if selected == label then " ✔" else "")
        pure $ const label <$> clicked

enableCameraCheckbox :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Bool)
enableCameraCheckbox baby' = do
  evEv <- dyn $ enableCameraCheckbox' baby' <$> baby'^.videoDevices
  switchPromptly never evEv


enableCameraCheckbox' :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> [MediaDeviceInfo] -> m (Event t Bool)
enableCameraCheckbox' baby' videoDevices' =
  case videoDevices' of
    [] -> pure never -- No need to enable the camera when there is none!
    _  -> myCheckBox  ("class" =: "cam-on ") (baby'^.cameraEnabled) $ text "\xf03d"

enableAutoStartCheckbox :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Bool)
enableAutoStartCheckbox baby' =
    myCheckBox ("class" =: "autostart ") (baby'^.autoStartEnabled) $ text $ i18n EN Autostart

setBabyNameForm :: forall m t. (HasWebView m, MonadWidget t m)
                   => App.Loaded t -> Baby t -> m (Event t Text)
setBabyNameForm loaded baby' = do
  (nameAddRequest, selectedName) <-
    elClass "div" "welcome-form baby-form" $ mdo
      elClass "span" "baby-form" $ text $ i18n EN Adjust_camera_for
      elClass "span" "baby-text" $ text "BABY"

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
  nameAdded <- editStringEl (pure nameAddRequest) (text $ i18n EN Add_new_baby_name) (constDyn "")
  pure $ leftmost [ selectedName, nameAdded ]

renderBabySelectors :: forall m t. (HasWebView m, MonadWidget t m)
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

displayScreenOnWarning :: forall m t. (HasWebView m, MonadWidget t m)
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
    displayWarning True = dismissibleOverlay "warning-overlay"  10 $ do
      text "For video to work, please do not switch off the screen!"
      el "br" blank
      text "Alternatively, if all you need is audio, please disable the camera."

autoStartActiveMessage :: forall m t. (HasWebView m, MonadWidget t m) => m (Event t ())
autoStartActiveMessage = do
  (disableEv, triggerDisable) <- newTriggerEvent
  dismissibleOverlay "info-overlay" 2 $ do
    text "Autostart active ..."
    el "br" blank
    el "br" blank
    clicked <- makeClickable . elAttr' "div" (addBtnAttrs "stop") $ text $ i18n EN Disable
    performEvent_ $ const (liftIO $ triggerDisable ())  <$> clicked
  pure disableEv


showPermissionError :: forall m t. (HasWebView m, MonadWidget t m) => Either UserMediaException MediaStream ->  m (Event t (), Event t ())
showPermissionError (Right _) = pure (never, never)
showPermissionError (Left _) = elClass "div" "fullScreenOverlay" $ do
    el "script" $ text "screenfull.exit();" -- Leave fullscreen so user sees the address bar.
    backClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
    el "h1" $ text "Error - so sad!"
    el "h2" $ text "Can not access your camera or microphone!"
    el "br" blank
    text "Obviously those are needed for a baby monitor. Please check your browser settings whether gonimo is allowed access!"
    el "br" blank
    el "br" blank
    isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
    isChrome <- getBrowserProperty "blink"
    when isChrome $ do
      if isMobile
        then text "Please click on the lock symbol in the browser address bar for accessing the settings."
        else text "Please click on the lock or the camera symbol in the browser address bar for accessing the settings."
    el "br" blank
    el "br" blank
    retry <- makeClickable . elAttr' "div" (addFullScreenBtnAttrs "btn-lang") $ text "TRY AGAIN"
    pure (backClicked, retry)

