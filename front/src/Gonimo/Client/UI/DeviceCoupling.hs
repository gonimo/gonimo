{-|
Module      : Gonimo.Client.UI.DeviceCoupling
Description : Initial device coupling screen for first time users.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.UI.DeviceCoupling where

import           Reflex.Dom.Core


import           Gonimo.Client.Prelude
import           Gonimo.Client.Model               (IsConfig)
import           Gonimo.Client.Reflex.Dom
import qualified Gonimo.Client.UI.Dialogs.Invite as Invite


-- type HasModel model = Invite.HasModel model
type HasModelConfig c t = (IsConfig c t, Invite.HasModelConfig c t)

type HasModel model = Invite.HasModel model

ui :: forall model mConf m t. (HasModelConfig mConf t, HasModel model, GonimoM model t m) => m (mConf t)
ui = do
  (inviteClicked, acceptInviteClicked) <-
    elClass "main" "container" $ do
    -- elClass "header" "mdc-toolbar mdc-toolbar--fixed" $ do
    --   elClass "div" "mdc-toolbar__row" $ do
    --     -- left Toolbar
    --     elClass "section" "mdc-toolbar__section mdc-toolbar__section--align-start" $ do
    --       elAttr "i" ( "class" =: "material-icons mdc-toolbar__menu-icon btn" <> "id" =: "nav-drawer" ) $ text "menu" -- button for nv drawer
    --       elClass "div" "mdc-menu-anchor" $ do
    --         elClass "div" "family-toggle btn" $ do
    --           elClass "i" "material-icons mdc-toolbar__menu-icon" $ text "group"
    --           elAttr "span" ("class" =: "mdc-toolbar__title" <> "id" =: "current-family") $ text "Family_1" -- current family
    --         elAttr "div" ("class" =: "mdc-simple-menu" <> "tabindex" =: "-1" <> "id" =: "family-menu") $ do
    --           elAttr "ul" ("class" =: "mdc-simple-menu__items mdc-list" <> "role" =: "menu" <> "aria-hidden" =: "true") $ do
    --             elAttr "a" ("class" =: "mdc-list-item btn" <> "role" =: "menuitem" <> "tabindex" =: "0") $ text "Family_1" -- current family should be the first item

      elClass "div" "mdc-layout-grid max-width" $ do
        elClass "div" "mdc-layout-grid__inner" $ do
          elClass "div" "mdc-card mdc-layout-grid__cell mdc-layout-grid__cell--span-12 invite" $ do
            elClass "section" "mdc-card__primary" $ do
              elClass "h1" "mdc-card__title mdc-card__title--large" $ text "Gerätekopplung"
            elClass "section" "invite-img-wrapper" $ elAttr "video" ( "class" =: "invite-img" <> "autoplay" =: "true" <> "loop" =: "true" <> "playsinline"  =: "true") $ do
              elAttr "source" ("src" =: "/pix/bear.mp4" <> "type" =: "video/mp4") blank
            elClass "section" "mdc-card__supporting-text" $ text "Erklärung zur Kopplung."
            elClass "section" "mdc-card__actions" $ do
              inviteClicked <-
                buttonClass "mdc-button mdc-button--raised mdc-card__action btn" $ do
                  elAttr "i" ("class" =: "material-icons" <> "aria-hidden" =: "true") $ text "code"
                  text "Code generieren"
              acceptInviteClicked <-
                buttonClass "mdc-button mdc-button--raised mdc-card__action btn" $ do
                  elAttr "i" ("class" =: "material-icons" <> "aria-hidden" =: "true") $ text "input"
                  text "Code eingeben"
              pure (inviteClicked, acceptInviteClicked)
  Invite.ui $ Invite.Config { Invite._onOpen = inviteClicked
                            , Invite._onClose = never
                            }
