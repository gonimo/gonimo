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
import           Reflex.Dom

import qualified Data.Map                          as Map
import qualified Gonimo.Client.App.Types           as App
import           Gonimo.Client.Baby.Internal
import qualified Gonimo.Client.Baby.Socket         as Socket
import qualified Gonimo.Client.NavBar              as NavBar
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server              (webSocket_recv)
import           Gonimo.DOM.Navigator.MediaDevices
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
                          , App._request = baby'^.socket.Socket.request
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
    navBar <- NavBar.navBar (NavBar.Config loaded deviceList NavBar.NoConfirmation NavBar.NoConfirmation)
    elClass "div" "container absoluteReference" $ do
      _ <- dyn $ renderVideo <$> baby'^.mediaStream
      elClass "div" "videoOverlay fullContainer" $ do
        elClass "div" "vCenteredBox" $ do
          enableCamera <- enableCameraCheckbox baby'
          selectCamera <- cameraSelect baby'
          startClicked <- buttonAttr ("class" =: "btn btn-lg btn-success") $ do
            text "Start "
            elClass "span" "glyphicon glyphicon-ok" blank
          pure $ UI { _uiGoHome = leftmost [ navBar^.NavBar.homeClicked, navBar^.NavBar.backClicked ]
                    , _uiStartMonitor = startClicked
                    , _uiStopMonitor = never -- already there
                    , _uiEnableCamera = enableCamera
                    , _uiSelectCamera = selectCamera
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
    _   -> do
            enabledElClass "div" "dropdown" (baby'^.cameraEnabled)$ do
              elAttr "button" ( "class" =: "btn btn-default dropdown-toggle"
                                <> "type" =: "button"
                                <> "id" =: "cameraSelectBaby"
                                <> "data-toggle" =: "dropdown"
                              ) $ do
                text " "
                dynText selectedCameraText
                text " "
                elClass "span" "caret" blank
              elClass "ul" "dropdown-menu" $ renderCameraSelectors
  where
    selectedCameraText = fromMaybe "" <$> baby'^.selectedCamera
    enabledElClass name className enabled =
      let
        attrDyn = (\on -> if on
                          then "class" =: className
                          else "class" =: (className <> " disabled")) <$> enabled
      in
        elDynAttr name $ attrDyn

    videoMap = pure . Map.fromList $ zip
                (baby'^.videoDevices.to (map mediaDeviceLabel))
                (baby'^.videoDevices)

    renderCameraSelectors
      = fmap fst <$> selectViewListWithKey selectedCameraText videoMap renderCameraSelector


    renderCameraSelector :: Text -> Dynamic t MediaDeviceInfo -> Dynamic t Bool ->  m (Event t ())
    renderCameraSelector label _ selected' = do
      elAttr "li" ("role" =: "presentation" <> "data-toggle" =: "collapse") $ do
        fmap (domEvent Click . fst )
        . elAttr' "a" ( "role" =: "menuitem"
                        <> "tabindex" =: "-1" <> "href" =: "#"
                      ) $ do
          text label
          dynText $ ffor selected' (\selected -> if selected then " ✔" else "")

enableCameraCheckbox :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Bool)
enableCameraCheckbox baby' =
  case baby'^.videoDevices of
    [] -> pure never -- No need to enable the camera when there is none!
    _  -> do
      myCheckBox ("class" =: "btn btn-default") (baby'^.cameraEnabled) $
        dynText $ makeEnableText <$> baby'^.cameraEnabled
  where
    makeEnableText :: Bool -> Text
    makeEnableText False = "Enable Camera"
    makeEnableText True = "Disable Camera"
