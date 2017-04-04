{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Control.Monad
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
import           Gonimo.Client.ConfirmationButton  (addConfirmation)
import           Gonimo.Client.Prelude
import           Gonimo.Client.Util

data BabyScreen = ScreenStart | ScreenRunning

ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> m (App.Screen t)
ui appConfig loaded deviceList = mdo
    baby' <- baby $ Config { _configSelectCamera = ui'^.uiSelectCamera
                           , _configEnableCamera = ui'^.uiEnableCamera
                           , _configEnableAutoStart = ui'^.uiEnableAutoStart
                           , _configResponse = appConfig^.App.server.webSocket_recv
                           , _configAuthData = loaded^.App.authData
                           , _configStartMonitor = startMonitor
                           , _configStopMonitor  = leftmost [ui'^.uiGoHome, ui'^.uiStopMonitor]
                           , _configSetBabyName = ui'^.uiSetBabyName
                           , _configSelectedFamily = loaded^.App.selectedFamily
                           }

    (autoStartEv, triggerAutoStart) <- newTriggerEvent
    doAutoStart <- readAutoStart

    uiDyn <- widgetHold (uiStart loaded deviceList baby') (renderCenter baby' <$> screenSelected)

    liftIO $ when doAutoStart $ triggerAutoStart ()

    let startMonitor = leftmost [ ui'^.uiStartMonitor
                                , autoStartEv
                                ]

    let ui' = uiSwitchPromptlyDyn uiDyn

    let screenSelected = leftmost [ const ScreenStart <$> ui'^.uiStopMonitor
                                  , const ScreenRunning <$> startMonitor
                                  ]

    performEvent_ $ const (do
                              cStream <- sample $ current (baby'^.mediaStream)
                              stopMediaStream cStream
                          ) <$> ui'^.uiGoHome
    let babyApp = App.App { App._subscriptions = baby'^.socket.Socket.subscriptions
                          , App._request = baby'^.socket.Socket.request <> baby'^.request
                          }
    pure $ App.Screen { App._screenApp = babyApp
                      , App._screenGoHome = ui'^.uiGoHome
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
        newBabyName <-
          setBabyNameForm loaded baby'
        _ <- dyn $ renderVideo <$> baby'^.mediaStream
        startClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Start"
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
                    }
  where
    renderVideo stream
      = elClass "div" "stream-baby" $
        mediaVideo stream ( "autoplay" =: "true"
                          <> "muted" =: "true"
                          )

uiRunning :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Loaded t -> DeviceList.DeviceList t -> Baby t -> m (UI t)
uiRunning loaded deviceList baby' =
  elClass "div" "container" $ mdo
    displayScreenOnWarning baby'
    dayNight <- holdDyn "night" $ tag toggledDayNight dayNightClicked
    let
      toggledDayNight :: Behavior t Text
      toggledDayNight = (\c -> if c == "day" then "night" else "day") <$> current dayNight
    let
      babyClass :: Dynamic t Text
      babyClass = pure "baby setup-done " <> dayNight

    (ui', dayNightClicked) <-
      elDynClass "div" babyClass $ do
        elClass "div" "good-night" $ do
          el "h1" $ text "Good Night"
          el "h2" $ dynText $ baby'^.name <> pure "!"
        elClass "div" "fill-full-screen" blank
        _ <- dyn $ noSleep <$> baby'^.mediaStream
        let
          leaveConfirmation :: forall m1. (HasWebView m1, MonadWidget t m1) => m1 ()
          leaveConfirmation = do
              el "h3" $ text "Really stop baby monitor?"
              el "p" $ text "All connected devices will be disconnected!"

        navBar' <- NavBar.navBar (NavBar.Config loaded deviceList)

        navBar <- NavBar.NavBar
                  <$> addConfirmation leaveConfirmation (navBar'^.NavBar.backClicked)
                  <*> addConfirmation leaveConfirmation (navBar'^.NavBar.homeClicked)

        dayNightClicked' <- makeClickable . elAttr' "div" (addBtnAttrs "time") $ blank
        stopClicked <- elClass "div" "stream-menu" $
          addConfirmation leaveConfirmation
          =<< (makeClickable . elAttr' "div" (addBtnAttrs "stop") $ text "Stop")
        -- stopClicked <- addConfirmation leaveConfirmation
        --               =<< (makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Stop")

        let ui'' = UI { _uiGoHome = navBar^.NavBar.homeClicked
                      , _uiStartMonitor = never
                      , _uiStopMonitor = leftmost [navBar^.NavBar.backClicked, stopClicked]
                      , _uiEnableCamera = never
                      , _uiEnableAutoStart = never
                      , _uiSelectCamera = never
                      , _uiSetBabyName = never
                      }
        pure (ui'', dayNightClicked')
    pure ui'
  where
    noSleep stream
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
    selectedCameraText = fromMaybe "Standard Setting" <$> baby'^.selectedCamera

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
    _  -> myCheckBox (addBtnAttrs "cam-on ") (baby'^.cameraEnabled) $ el "span" blank

enableAutoStartCheckbox :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Bool)
enableAutoStartCheckbox baby' =
    myCheckBox (addBtnAttrs "autostart ") (baby'^.autoStartEnabled) . el "span" $ text "Autostart"

setBabyNameForm :: forall m t. (HasWebView m, MonadWidget t m)
                   => App.Loaded t -> Baby t -> m (Event t Text)
setBabyNameForm loaded baby' = --mdo
  -- (clicked, nameAdded) <- do
  elClass "div" "welcome-form baby-form" $ mdo
    elClass "span" "baby-form" $ text "Adjust camera for"

    clicked <-
      makeClickable . elAttr' "div" (addBtnAttrs "family-select") $ do
        dynText $ baby'^.name
        text " "
        elClass "span" "caret" blank

    nameAdded <-
      editStringEl (makeClickable $ elAttr' "div" (addBtnAttrs "input-btn plus baby-form") blank)
      (text "Add new baby name ...")
      (constDyn "")
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
    pure $ leftmost [ nameAdded
                    , selectedName
                    ]

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

-- copy & paste for now. mimimimi ....
renderVolumemeter :: forall m t. (HasWebView m, MonadWidget t m) => Event t Double -> m ()
renderVolumemeter volEvent = do
    elClass "div" "volumemeter" $ do
      volDyn <- holdDyn 0 volEvent
      traverse_ (renderVolBarItem volDyn) [0, 0.1 .. 0.9]
  where
    renderVolBarItem :: Dynamic t Double -> Double -> m ()
    renderVolBarItem currentVolume minVal = do
      let isActive = uniqDyn $ (\cv -> if cv > minVal then "volBarItemActive" else "") <$> currentVolume
      elDynClass "div" ( pure "volBarItem " <> isActive) $ blank

displayScreenOnWarning :: forall m t. (HasWebView m, MonadWidget t m)
            => Baby t -> m ()
displayScreenOnWarning baby' = do
  isMobile <- getBrowserProperty "mobile"
  isTablet <- getBrowserProperty "tablet"
  let haveVideo = baby'^.cameraEnabled
  let needWarning = (&& (isMobile || isTablet)) <$> haveVideo

  dismissedEv <- delayed 10 -- Hide after 10 seconds
  dismissed <- holdDyn False $ const True <$> dismissedEv
  let shown = (&&) <$> needWarning <*> (not <$> dismissed)
  _ <- dyn $ displayWarning <$> shown
  pure ()
  where
    displayWarning False = pure ()
    displayWarning True = elClass "div" "keep-screen-on-warning" $ do
      text "For video to work, please don't switch off the screen."
      el "br" blank
      text "Alternatively, if all you need is audio, please switch off the camera."
