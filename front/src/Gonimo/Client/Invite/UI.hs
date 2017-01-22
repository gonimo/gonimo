{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
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
import Gonimo.Types (InvitationDelivery(..))
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.SocketAPI.Types as API
import Gonimo.Client.Reflex.Dom

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, MonadIO (Performable m), PerformEvent t m)
      => Config t -> m (Invite t)
ui config = mdo
    baseUrl <- getBaseLink
    (createInvEv, makeCreateInvEv) <- newTriggerEvent
    liftIO $ makeCreateInvEv () -- We want a new invitation every time this widget is rendered
    invite' <- invite $ config & configCreateInvitation .~ leftmost (createInvEv : (fmap (const ()) <$> sentEvents))
    let invite'' = invite' & request %~ (mailReqs' <>)

    let currentInvitation = invite''^.invitation
    let invitationLink = maybe "" (makeInvitationLink baseUrl . snd) <$> currentInvitation
    let escapedLink = T.decodeUtf8 . urlEncode True . T.encodeUtf8 <$> invitationLink
    let sentEvents = (const SentEmail <$> mailReqs') : reCreateEvents

    (reCreateEvents, mailReqs') <- divClass "container" $ do
      confirmationBox $ leftmost sentEvents
      invButtons <- elAttr "div" ("style" =: "display:flex; justify-content: space-between;") $ do
        elAttr "div" ("class" =: "btn-group btn-group-justified" <> "role" =: "group") $ do
          whatsAppClicked <- inviteButton "/pix/WhatsApp.png" "WhatsApp" "whatsapp://send?text=" escapedLink
          tgClicked <- inviteButton "/pix/Telegram.png" "Telegram" "tg://msg?text=" escapedLink
          pure [const SentWhatsApp <$> whatsAppClicked, const SentTelegram <$> tgClicked]
      mailReqs <- divClass "row" $ emailWidget (config^.configResponse) currentInvitation
      recreateClicked <- divClass "row" $ do
        copyClipboardScript
        divClass "input-group" $ do
            clicked <- refreshLinkButton
            showLinkInput invitationLink
            copyClicked <- copyButton
            pure [const SentRefresh <$> clicked, const SentCopy <$> copyClicked]
      pure $ (recreateClicked <> invButtons, mailReqs)
    pure invite''
  where

    inviteButton img name linkBase payload =
      elAttr "div" ("class" =: "btn-group" <> "role" =: "group"
                    <> "style" =: "height: 20vh;") $ do
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

confirmationBox :: forall m t. (DomBuilder t m, MonadIO (Performable m), MonadHold t m
                               , TriggerEvent t m, PerformEvent t m)
                   => Event t InvitationSent -> m ()
confirmationBox sent = do
  let mSent = Just <$> sent
  closeEvent <- fmap (const Nothing) <$> delay 20 mSent
  let lifeEvent = leftmost [mSent, closeEvent]
  _ <- widgetHold (pure ()) $ confirmationBox' <$> lifeEvent
  pure ()

confirmationBox' :: forall m t. (DomBuilder t m) => Maybe InvitationSent -> m ()
confirmationBox' Nothing = pure ()
confirmationBox' (Just sendMethod) = do
  elClass "div" "alert alert-success" $ do
    case sendMethod of
      SentWhatsApp -> el "strong" $ text "Sent with WhatsApp!"
      SentTelegram -> el "strong" $ text "Sent with Telegram!"
      SentCopy     -> el "strong" $ text "Copied invitation link to clipboard!"
      SentRefresh  -> el "strong" $ text "New invitation generated!"
      SentEmail    -> el "strong" $ text "Sent email!"
    el "br" blank
    el "h1" $ text "Next steps:"
    elClass "ul" "list-group" $ do
      elClass "li" "list-group-item" $ text "Open invitation link on another device and press <Accept>"
      elClass "li" "list-group-item" $ text "Make one device a baby station"
      elClass "li" "list-group-item" $ text "Make the other device a parent station"

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

emailWidget :: forall t m. (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m
                           , MonadHold t m)
  => Event t API.ServerResponse -> Dynamic t (Maybe (InvitationId, Db.Invitation))
  -> m (Event t [API.ServerRequest])
emailWidget res invData = mdo
    (infoBox', req) <- elAttr "div" ("class" =: "input-group" <> "style" =: "width:100%;") $ do
      awesomeAddon "fa-envelope"
      addrInput <- textInput $ def { _textInputConfig_attributes = (pure $ "placeholder" =: "mail@example.com"
                                                                       <> "class" =: "form-control"
                                                                   )
                                   , _textInputConfig_inputType = "email"
                                   }
      let addr = addrInput^.textInput_value
      sendClicked <- sendEmailBtn
      let enterPressed' = enterPressed $ addrInput^.textInput_keypress
      let sendRequested = leftmost [sendClicked, enterPressed']
      let infoBox = widgetHold (pure ()) $ showSuccess <$> leftmost [ invSent, const Nothing <$> sendRequested ]
      pure $ (infoBox, buildRequest sendRequested (current addr))
    _ <- infoBox'
    pure req
  where
    buildRequest :: Event t () -> Behavior t Text -> Event t [API.ServerRequest]
    buildRequest clicked addr =
      let
        makeReqs mId email = maybe [] (:[])
          $ API.ReqSendInvitation <$> (API.SendInvitation <$> mId <*> Just (EmailInvitation email))
        mInvId = fmap fst <$> current invData
        invAddr = (,) <$> mInvId <*> addr
      in
        uncurry makeReqs <$> tag invAddr clicked

    sendEmailBtn =
      fmap (domEvent Click . fst)
        $ elAttr "span" ("class" =: "input-group-btn") $
            elAttr' "button" ("class" =: "btn btn-default" <> "type" =: "button") $
              text "Send Email"

    invSent :: Event t (Maybe API.SendInvitation)
    invSent = push (\r -> pure $ case r of
                                   API.ResSentInvitation inv -> Just (Just inv)
                                   _ -> Nothing
                   ) res

    showSuccess Nothing = pure ()
    showSuccess (Just inv) = do
        elAttr "div" ("class" =: "alert alert-success") $
          text $ "e-mail successfully sent to: " <> getAddr inv
      where
        getAddr (API.SendInvitation _ (EmailInvitation addr)) = addr
        getAddr _ = "nobody"
