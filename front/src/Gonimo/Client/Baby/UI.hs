{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

cameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                => Baby t -> m ()
cameraSelect baby' = do
    elClass "div" "dropdown" $ do
      elAttr "button" ( "class" =: "btn btn-default dropdown-toggle"
                        <> "type" =: "button"
                        <> "id" =: "cameraSelectBaby"
                        <> "data-toggle" =: "dropdown"
                      ) $ do
        text "bliblablueh"
        elClass "span" "caret" blank
      elClass "ul" "dropdown-menu" $ do
        sequence_ $ zipWith renderCamera [1..] (baby'^. videoDevices)
  where
    renderCamera :: Int -> MediaDeviceInfo -> m ()
    renderCamera num mediaInfo = do
      elAttr "ul" ("role" =: "presentation") $ do
        elAttr "a" ("role" =: "menuitem" <> "tabindex" =: "-1" <> "href" =: "#") $
          text $ if mediaDeviceLabel mediaInfo == ""
                 then "Camera " <> (T.pack . show) num
                 else mediaDeviceLabel mediaInfo

-- Overrides configCreateBaby && configLeaveBaby
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => Config t -> m ()
ui config = do
  window  <- DOM.currentWindowUnchecked
  navigator <- Window.getNavigatorUnsafe window
  constr <- makeDefaultUserMediaDictionary
  _ <- Navigator.getUserMedia navigator $ Just constr
  baby' <- baby config
  elClass "div" "container" $ 
    cameraSelect baby'
