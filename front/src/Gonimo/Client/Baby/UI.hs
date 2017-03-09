{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Gonimo.Client.DeviceList          as DeviceList
import           Reflex.Dom.Core

import qualified Data.Map                          as Map
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.SocketAPI                  as API
import           Gonimo.Client.Baby.Internal
import qualified Gonimo.Client.Baby.Socket         as Socket
import qualified Gonimo.Client.NavBar              as NavBar
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server              (webSocket_recv)
import           Gonimo.DOM.Navigator.MediaDevices
import           Gonimo.Client.Prelude
import           Gonimo.Client.EditStringButton    (editStringEl)
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM as DOM
-- import           Gonimo.Client.ConfirmationButton  (confirmationButton)

data BabyScreen = ScreenStart | ScreenRunning

ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> DeviceList.DeviceList t -> m (App.Screen t)
ui appConfig loaded deviceList = mdo
    baby' <- baby $ Config { _configSelectCamera = ui'^.uiSelectCamera
                           , _configEnableCamera = ui'^.uiEnableCamera
                           , _configResponse = appConfig^.App.server.webSocket_recv
                           , _configAuthData = loaded^.App.authData
                           , _configStartMonitor = ui'^.uiStartMonitor
                           , _configStopMonitor  = ui'^.uiStopMonitor
                           }

    uiDyn <- widgetHold (uiStart loaded deviceList baby') (renderCenter baby' <$> screenSelected)

    let ui' = uiSwitchPromptlyDyn uiDyn

    let screenSelected = leftmost [ const ScreenStart <$> ui'^.uiStopMonitor
                                  , const ScreenRunning <$> ui'^.uiStartMonitor
                                  ]

    performEvent_ $ const (do
                              cStream <- sample $ current (baby'^.mediaStream)
                              stopMediaStream cStream
                          ) <$> ui'^.uiGoHome
    let babyApp = App.App { App._subscriptions = baby'^.socket.Socket.subscriptions
                          , App._request = baby'^.socket.Socket.request <> ui'^.uiRequest
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
      navBar <- NavBar.navBar (NavBar.Config loaded deviceList NavBar.NoConfirmation NavBar.NoConfirmation)
      elClass "div" "baby" $ mdo
        (setBabyNameReq, cBabyName) <-
          setBabyNameForm loaded startClicked
        _ <- dyn $ renderVideo <$> baby'^.mediaStream
        elClass "div" "time" blank
        startClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ text "Done"
        elClass "div" "stream-menu" $ do
          selectCamera <- cameraSelect baby'
          enableCamera <- enableCameraCheckbox baby'
          pure $ UI { _uiGoHome = leftmost [ navBar^.NavBar.homeClicked, navBar^.NavBar.backClicked ]
                    , _uiStartMonitor = startClicked
                    , _uiStopMonitor = never -- already there
                    , _uiEnableCamera = enableCamera
                    , _uiSelectCamera = selectCamera
                    , _uiRequest = setBabyNameReq
                    }
  where
    renderVideo stream
      = mediaVideo stream ( "style" =: "height:100%; width:100%"
                            <> "autoplay" =: "true"
                            <> "muted" =: "true"
                          )

uiRunning :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Loaded t -> DeviceList.DeviceList t -> Baby t -> m (UI t)
uiRunning loaded deviceList baby' = do
    _ <- dyn $ noSleep <$> baby'^.mediaStream
    let
      leaveConfirmation :: forall m1. (HasWebView m1, MonadWidget t m1) => m1 ()
      leaveConfirmation = do
          el "h3" $ text "Really stop baby monitor?"
          el "p" $ text "All connected devices will be disconnected!"

    let navConfirmation = NavBar.WithConfirmation leaveConfirmation
    navBar <- NavBar.navBar (NavBar.Config loaded deviceList navConfirmation navConfirmation)
    cuteBunny
    -- TODO: As confirmation button this triggers: Maybe.fromJust: Nothing! WTF!
    -- stopClicked' <- confirmationButton ("class" =: "btn btn-lg btn-danger")
    --                 ( do
    --                     text "Stop "
    --                     elClass "span" "glyphicon glyphicon-off" blank
    --                 )
    --                 leaveConfirmation
    -- let stopClicked = traceEvent "_bad_click_" stopClicked'

    stopClicked <- buttonAttr ("class" =: "btn btn-lg btn-danger")
                    ( do
                        text "Stop "
                        elClass "span" "glyphicon glyphicon-off" blank
                    )
    let goBack = leftmost [ stopClicked, navBar^.NavBar.backClicked ]
    -- let leave = leftmost [ navBar^.NavBar.homeClicked, goBack ]


    pure $ UI { _uiGoHome = navBar^.NavBar.homeClicked
              , _uiStartMonitor = never
              , _uiStopMonitor = goBack
              , _uiEnableCamera = never
              , _uiSelectCamera = never
              , _uiRequest = never
              }
  where
    noSleep stream
      = mediaVideo stream ( "style" =: "display:none"
                            <> "autoplay" =: "true"
                            <> "muted" =: "true"
                          )
    cuteBunny = elAttr "img" ( "alt" =: "gonimo"
                    <> "src" =: "pix/gonimo-brand-01.svg"
                    <> "height" =: "100%"
                    ) blank


cameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Text)
cameraSelect baby' =
  case baby'^.videoDevices of
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
              dropDownClass = pure "baby-form welcome-form dropUp-container .container " <> droppedDownClass

            selectedName <-
              elDynClass "div" dropDownClass $ renderCameraSelectors cameras
            pure selectedName
  where
    selectedCameraText = fromMaybe "Standard Setting" <$> baby'^.selectedCamera

    cameras = baby'^.videoDevices.to (map mediaDeviceLabel)

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
enableCameraCheckbox baby' =
  case baby'^.videoDevices of
    [] -> pure never -- No need to enable the camera when there is none!
    _  -> myCheckBox (addBtnAttrs "cam-on ") (baby'^.cameraEnabled) $ el "span" blank

setBabyNameForm :: forall m t. (HasWebView m, MonadWidget t m)
                   => App.Loaded t -> Event t () -> m (Event t [ API.ServerRequest ], Dynamic t Text)
setBabyNameForm loaded started = --mdo
  -- (clicked, nameAdded) <- do
  elClass "div" "welcome-form baby-form" $ mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mInitial <- GStorage.getItem storage GStorage.lastBabyName
    cBabyName <- holdDyn (fromMaybe "baby" mInitial) $ leftmost [nameAdded, selectedName]
    performEvent_
      $ GStorage.setItem storage GStorage.lastBabyName <$> updated cBabyName

    elClass "span" "baby-form" $ text "Adjust camera for"

    clicked <-
      makeClickable . elAttr' "div" (addBtnAttrs "family-select") $ do
        dynText $ cBabyName
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
      dropDownClass = pure "baby-form welcome-form dropDown-container .container " <> droppedDownClass

    selectedName <-
      elDynClass "div" dropDownClass $ renderBabySelectors (App.babyNames loaded)
    let
      mkRequest = API.ReqSaveBabyName <$> current (loaded^.App.selectedFamily)
    let req = fmap (:[]) . attachWith ($) mkRequest $ leftmost [ nameAdded
                                                               , selectedName
                                                               , tag (current cBabyName) started
                                                               ]
    pure (req, cBabyName)

renderBabySelectors :: forall m t. (HasWebView m, MonadWidget t m)
                    => Dynamic t [Text] -> m (Event t Text)
renderBabySelectors names =
  let
    renderBabySelector :: Text -> m (Event t Text)
    renderBabySelector name = do
        fmap (fmap (const name)) . el "div" $ do
          makeClickable . elAttr' "a" (addBtnAttrs "") $ text name

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
