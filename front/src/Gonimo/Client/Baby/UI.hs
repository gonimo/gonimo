{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Data.Map                          (Map)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Gonimo.Client.DeviceList          as DeviceList
import qualified Gonimo.Client.Invite              as Invite
import           Gonimo.Client.Reflex
import qualified Gonimo.Db.Entities                as Db
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.Types                      as Gonimo
import           Reflex.Dom

import           Data.Foldable
import qualified Data.Text                         as T
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
-- import qualified JSDOM.Custom.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.Client.Auth                as Auth
import           Gonimo.Client.Baby.Internal
import           Gonimo.Client.ConfirmationButton  (confirmationButton)
import           Gonimo.Client.EditStringButton    (editStringButton)
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server              (webSocket_recv)
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Data.Map as Map
import Gonimo.DOM.Navigator.MediaDevices

cameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m (Event t Text)
cameraSelect baby' = do
    elClass "div" "dropdown" $ do
      elAttr "button" ( "class" =: "btn btn-default dropdown-toggle"
                        <> "type" =: "button"
                        <> "id" =: "cameraSelectBaby"
                        <> "data-toggle" =: "dropdown"
                      ) $ do
        text " "
        dynText $ baby'^.selectedCamera
        text " "
        elClass "span" "caret" blank
      elClass "ul" "dropdown-menu" $ renderCameraSelectors
  where
    videoMap = pure . Map.fromList $ zip
                (baby'^.videoDevices.to (map mediaDeviceLabel))
                (baby'^.videoDevices)

    renderCameraSelectors
      = fmap fst <$> selectViewListWithKey (baby'^.selectedCamera) videoMap renderCameraSelector


    renderCameraSelector :: Text -> Dynamic t MediaDeviceInfo -> Dynamic t Bool ->  m (Event t ())
    renderCameraSelector label _ selected' = do
      elAttr "li" ("role" =: "presentation" <> "data-toggle" =: "collapse") $ do
        fmap (domEvent Click . fst )
        . elAttr' "a" ( "role" =: "menuitem"
                        <> "tabindex" =: "-1" <> "href" =: "#"
                      ) $ do
          text label
          dynText $ ffor selected' (\selected -> if selected then " ✔" else "")

-- Overrides configCreateBaby && configLeaveBaby
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => m ()
ui = mdo
    baby' <- baby $ Config { _configSelectCamera = cameraSelected }
    cameraSelected <- elClass "div" "container absoluteReference" $ do
      _ <- dyn $ renderVideo <$> baby'^.mediaStream
      elClass "div" "videoOverlay fullContainer" $ do
        elClass "div" "vCenteredBox" $ do
          cameraSelect baby'
    pure ()
  where
    renderVideo stream
      = mediaVideo stream ( "style" =: "height:100%; width:100%"
                            <> "autoplay" =: "true"
                            <> "muted" =: "true"
                          )

