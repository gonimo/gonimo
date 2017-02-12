{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Baby.UI where

import           Control.Lens
import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Gonimo.Client.DeviceList         as DeviceList
import qualified Gonimo.Client.Invite             as Invite
import           Gonimo.Client.Reflex
import qualified Gonimo.Db.Entities               as Db
import qualified Gonimo.SocketAPI                 as API
import qualified Gonimo.Types                     as Gonimo
import           Reflex.Dom

import           Data.Foldable
import qualified Gonimo.Client.App.Types          as App
import qualified Gonimo.Client.Auth               as Auth
import           Gonimo.Client.Baby.Internal
import           Gonimo.Client.ConfirmationButton (confirmationButton)
import           Gonimo.Client.EditStringButton   (editStringButton)
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server             (webSocket_recv)

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
        traverse_ renderCamera (baby'^. videoDevices)
  where
    renderCamera :: (Text, Text) -> m ()
    renderCamera (_, devLabel) = do
      elAttr "ul" ("role" =: "presentation") $ do
        elAttr "a" ("role" =: "menuitem" <> "tabindex" =: "-1" <> "href" =: "#") $
          text $ if devLabel == "" then "The One and Only" else devLabel

-- Overrides configCreateBaby && configLeaveBaby
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => Baby t -> m ()
ui baby' = do
  elClass "div" "container" $ 
    cameraSelect baby'
