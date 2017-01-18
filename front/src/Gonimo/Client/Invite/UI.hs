{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Invite.UI where

import Reflex.Dom
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId, InvitationId)
import qualified Gonimo.Db.Entities as Db
import Gonimo.Client.Invite.Internal
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (maybe)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (urlEncode)

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m)
      => Config t -> m (Invite t)
ui config = mdo
    baseUrl <- getBaseLink
    (createInvEv, makeCreateInvEv) <- newTriggerEvent
    liftIO $ makeCreateInvEv () -- We want a new invitation every time this widget is rendered
    let invite' = invite $ config & configCreateInvitation .~ leftmost (createInvEv : reCreateEvents)
    currentInvitation <- holdDyn Nothing $ Just <$> invite'^.invitation
    let invitationLink = maybe "" (makeInvitationLink baseUrl . snd) <$> currentInvitation
    let escapedLink = T.decodeUtf8 . urlEncode True . T.encodeUtf8 <$> invitationLink

    reCreateEvents <- divClass "container" $ do
      invButtons <- divClass "row" $ do
        elAttr "div" ("class" =: "btn-group btn-group-justified" <> "role" =: "group") $ do
          whatsAppClicked <- inviteButton "/pix/WhatsApp.png" "WhatsApp" "whatsapp://send?text=" escapedLink
          tgClicked <- inviteButton "/pix/Telegram.png" "Telegram" "tg://msg?text=" escapedLink
          pure [whatsAppClicked, tgClicked]
      divClass "row" $ do -- emailWidget currentInvitation
        elAttr "div" ("class" =: "input-group" <> "style" =: "width:100%;") $ do
          awesomeAddon "fa-envelope"
          elAttr "input" ("type" =: "text"
                          <> "class" =: "form-control"
                          <> "placeholder" =: "mail@example.com"
                        ) $ pure ()
          sendEmailBtn
      recreateClicked <- divClass "row" $ do
        copyClipboardScript
        divClass "input-group" $ do
            clicked <- refreshLinkButton
            showLinkInput invitationLink
            copyClicked <- copyButton
            pure [clicked, copyClicked]
      pure $ recreateClicked <> invButtons
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
        let clickAttr n attrs children' = domEvent Click . fst <$> elDynAttr' n attrs children'
        clickAttr "a" (mkBtnAttrs <$> payload) $ do
          elAttr "img" ("src" =: img <> "style" =: "height: 80%;") $ pure ()
          elAttr "div" ("style" =: "text-align: center") $
            text name

awesomeAddon :: forall m t. (DomBuilder t m) =>  Text -> m ()
awesomeAddon t =
  elAttr "span" ( "class" =: "input-group-addon") $
    elAttr "i" ("class" =: ("fa " <> t)) blank

copyButton :: forall t m. DomBuilder t m => m (Event t ())
copyButton
  = elAttr "span" ("class" =: "input-group-btn") $
    fmap (domEvent Click . fst)
    $ elAttr' "button" ( "class" =: "btn btn-default"
                         <> "type" =: "button"
                         <> "title" =: "Copy link to clipboard"
                         <> "onClick" =: "copyInvitationLink()"
                       ) $ elAttr "i" ( "class" =: "fa fa-copy") blank

refreshLinkButton :: forall t m. DomBuilder t m => m (Event t ())
refreshLinkButton
  = elAttr "span" ("class" =: "input-group-btn") $
  fmap (domEvent Click . fst)
    $ elAttr' "button" ( "class" =: "btn btn-default"
                         <> "type" =: "button"
                         <> "title" =: "Generate new link"
                       ) $ elAttr "i" ( "class" =: "fa fa-refresh") blank

showLinkInput :: forall t m. (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
showLinkInput invitationLink =
  let
    makeLinkAttrs link' = ( "type" =: "text"
                            <> "class" =: "form-control"
                            <> "readOnly" =: "true"
                            <> "id" =: "invitationLinkUrlInput"
                            <> "value" =: link'
                          )
  in
    elDynAttr "input" (makeLinkAttrs <$> invitationLink) blank


copyClipboardScript :: forall t m. (DomBuilder t m) => m ()
copyClipboardScript = el "script" $ text $
       "copyTextFromId = function(id_) {\n"
    <> "    try\n"
    <> "    {\n"
    <> "        var textBox = document.querySelector(\"#\" + id_);\n"
    <> "        textBox.select();\n"
    <> "        return document.execCommand('copy');\n"
    <> "    }\n"
    <> "    catch (e) {\n"
    <> "        return false;\n"
    <> "    }\n"
    <> "};\n"
    <> "copyInvitationLink = function () {\n"
    <> "   copyTextFromId(\"invitationLinkUrlInput\");\n"
    -- <> "   alert(\"invitationLinkUrlInput\");\n"
    <> "};\n"

-- emailWidget :: forall t m. (DomBuilder t m) => Dynamic t (Maybe (InvitationId, Db.Invitation)) -> m (Event t API.ServerRequest)
-- emailWidget Nothing = do
  
