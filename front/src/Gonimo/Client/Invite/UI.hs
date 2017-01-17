{-# LANGUAGE RecursiveDo #-}
module Gonimo.Client.Invite.UI where

import Reflex.Dom
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId)
import Gonimo.Client.Invite.Internal
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (maybe)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (urlEncode)

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m)
      => Config t -> m (Invite t)
ui config = do
  baseUrl <- getBaseLink
  (createInvEv, makeCreateInvEv) <- newTriggerEvent
  liftIO $ makeCreateInvEv () -- We want a new invitation every time this widget is rendered
  let invite' = invite $ config & configCreateInvitation .~ createInvEv
  currentInvitation <- holdDyn Nothing $ Just <$> invite'^.invitation
  let invitationLink = maybe "" (makeInvitationLink baseUrl . snd) <$> currentInvitation
  let escapedLink = T.decodeUtf8 . urlEncode True . T.encodeUtf8 <$> invitationLink

  divClass "container" $ do
    divClass "row" $ do
      elAttr "div" ("class" =: "btn-group btn-group-justified" <> "role" =: "group") $ do
        inviteButton "/pix/WhatsApp.png" "WhatsApp" "whatsapp://send?text=" escapedLink
        inviteButton "/pix/Telegram.png" "Telegram" "tg://msg?text=" escapedLink
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
        let makeLinkAttrs link = ( "type" =: "text"
                                  <> "class" =: "form-control"
                                  <> "readOnly" =: "true"
                                  <> "id" =: "invitationLinkUrlInput"
                                  <> "value" =: link
                                )
        elDynAttr "input" (makeLinkAttrs <$> invitationLink) blank
        awesomeAddon "fa-copy"
  pure invite'
  where
    sendEmailBtn =
      elAttr "span" ("class" =: "input-group-btn") $ do
        elAttr "button" ("class" =: "btn btn-default" <> "type" =: "button") $ do
          text "Send Email"
    inviteButton img name linkBase payload =
      elAttr "div" ("class" =: "btn-group col-xs6" <> "role" =: "group"
                    <> "style" =: "height: 40vh;") $ do
        let mkBtnAttrs link' = ("class" =: "btn btn-default" <> "style" =: "height: 100%;"
                                <> "role" =: "button"
                                <> "type" =: "button"
                                <> "href" =: (linkBase <> link')
                               )
        domEvent Click . fst <$> elDynAttr' "a" (mkBtnAttrs <$> payload) ( do
          elAttr "img" ("src" =: img <> "style" =: "height: 80%;") $ pure ()
          elAttr "div" ("style" =: "text-align: center") $
            text name
                                                                         )

awesomeAddon :: forall m t. (DomBuilder t m) =>  Text -> m ()
awesomeAddon t =
  elAttr "span" ( "class" =: "input-group-addon") $
    elAttr "i" ("class" =: ("fa " <> t)) blank
