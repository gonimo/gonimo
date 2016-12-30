{-# LANGUAGE RecursiveDo #-}
module Gonimo.Client.Invite where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Text (Text)

data InviteConfig
  = InviteConfig {}

invite :: forall x. Widget x ()
invite = do
  divClass "container" $ do
    divClass "row" $ do
      elAttr "div" ("class" =: "btn-group btn-group-justified" <> "role" =: "group") $ do
        inviteButton "/pix/WhatsApp.png"
        inviteButton "/pix/Telegram.png"
    divClass "row" $ do
      elAttr "div" ("class" =: "input-group" <> "style" =: "width:100%;") $ do
        awesomeAddon "fa-envelope"
        elAttr "input" ("type" =: "text"
                        <> "class" =: "form-control"
                        <> "placeholder" =: "mail@example.com"
                      ) $ pure ()
        sendEmailBtn
    divClass "row" $ do
      divClass "input-group" $ do
        elAttr "span" ("class" =: "input-group-btn") $ do
          elAttr "button" ( "class" =: "btn btn-default"
                            <> "type" =: "button"
                            <> "title" =: "Generate new link"
                          ) $ do
            elAttr "i" ( "class" =: "fa fa-refresh") blank
        elAttr "input" ( "type" =: "text"
                       <> "class" =: "form-control"
                       <> "readOnly" =: "true"
                       <> "id" =: "invitationLinkUrlInput"
                       ) blank
        awesomeAddon "fa-copy"

  where
    sendEmailBtn =
      elAttr "span" ("class" =: "input-group-btn") $ do
        elAttr "button" ("class" =: "btn btn-default" <> "type" =: "button") $ do
          text "Send Email"
    inviteButton img =
      elAttr "div" ("class" =: "btn-group col-xs6" <> "role" =: "group"
                    <> "style" =: "height: 40vh;") $ do
        elAttr "button" ("class" =: "btn btn-default" <> "style" =: "height: 100%;"
                         <> "role" =: "button"
                        ) $ do
          elAttr "img" ("src" =: img <> "style" =: "height: 80%;") $ pure ()


awesomeAddon :: forall x. Text -> Widget x ()
awesomeAddon t =
  elAttr "span" ( "class" =: "input-group-addon") $
    elAttr "i" ("class" =: ("fa " <> t)) blank
