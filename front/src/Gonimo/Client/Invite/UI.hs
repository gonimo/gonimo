{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Invite.UI where

import Reflex.Dom.Core
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (InvitationId)
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
import qualified Gonimo.Client.App.Types as App
import           GHCJS.DOM.Types (MonadJSM)
import           Gonimo.Client.Invite.UI.I18N
import           Gonimo.Client.Util
import           Gonimo.I18N
import qualified GHCJS.DOM.Element as Element

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadIO (Performable m), PerformEvent t m)
      => App.Loaded t -> Config t -> m (Invite t)
ui loaded config = mdo
    baseUrl <- getBaseLink
    (createInvEv, makeCreateInvEv) <- newTriggerEvent
    liftIO $ makeCreateInvEv () -- We want a new invitation every time this widget is rendered
    invite' <- invite $ config & configCreateInvitation .~ leftmost (createInvEv : (fmap (const ()) <$> sentEvents))
    let invite'' = invite' & request %~ (mailReqs <>)

    let currentInvitation = invite''^.invitation
    let invitationLink = maybe "" (makeInvitationLink baseUrl . snd) <$> currentInvitation
    let escapedLink = T.decodeUtf8 . urlEncode True . T.encodeUtf8 <$> invitationLink
    let sentEvents = (const SentEmail <$> mailReqs) : reCreateEvents

    linkGotSent <- uniqDyn <$> holdDyn False (leftmost [ const True <$> leftmost sentEvents
                                                       , const False <$> leftmost [ doneClicked
                                                                                  , backClicked
                                                                                  ]
                                                       ]
                                             )
    performEvent_ $ (\goForIt -> when goForIt $ Element.scrollIntoView rawDone True) <$> updated linkGotSent


    backClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank

    el "h1" $ text $ i18n EN Invite_More_Devices
    el "h2" . dynText $ (i18n EN . To_your_family) <$> App.currentFamilyName loaded

    confirmationBox $ leftmost sentEvents
    invButtons <- elClass "div" "invite-buttons" $ do
      isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
      let onMobile mEv = if isMobile then mEv else pure never
      whatsAppClicked <- onMobile $ inviteButton "whatsapp" "whatsapp://send?text=" escapedLink
      tgClicked <- onMobile $ inviteButton "telegram" "tg://msg?text=" escapedLink
      pure [const SentWhatsApp <$> whatsAppClicked, const SentTelegram <$> tgClicked]

    el "h3" $ text "EMAIL"
    mailReqs <- emailWidget (config^.configResponse) currentInvitation

    recreateClicked <- el "div" $ do
      copyClipboardScript
      el "h3" $ text "INVITATION LINK"
      divClass "mail-form link" $ do
          showLinkInput invitationLink
          clicked <- refreshLinkButton
          copyClicked <- copyButton
          pure [const SentRefresh <$> clicked, const SentCopy <$> copyClicked]
    let reCreateEvents = recreateClicked <> invButtons

    el "br" blank
    let doneClass linkGotSent' = if linkGotSent' then "btn-lang next-action" else "btn-lang"
    (doneBtn, _) <- elDynAttr' "div" (addBtnDynAttrs $ doneClass <$> linkGotSent)
                   $ text $ i18n EN Done
    el "br" blank
    let rawDone =  _element_raw doneBtn
    let doneClicked = domEvent Click doneBtn

    pure $ invite'' & uiGoBack .~ backClicked
                    & uiDone .~ doneClicked
  where
    inviteButton className linkBase payload = do
        let mkLinkAttrs link' = "href" =: (linkBase <> link')
        elAttr "div" (addBtnAttrs $ "invite-button " <> className) $
          makeClickable . elDynAttr' "a" (mkLinkAttrs <$> payload) $ blank

confirmationBox :: forall m t. (DomBuilder t m, MonadIO (Performable m), MonadHold t m
                               , TriggerEvent t m, PerformEvent t m)
                   => Event t InvitationSent -> m ()
confirmationBox sent = do
  let mSent = Just <$> sent
  closeEvent <- fmap (const Nothing) <$> delay 2 mSent
  let lifeEvent = leftmost [mSent, closeEvent]
  _ <- widgetHold (pure ()) $ confirmationBox' <$> lifeEvent
  pure ()

confirmationBox' :: forall m t. (DomBuilder t m) => Maybe InvitationSent -> m ()
confirmationBox' Nothing = pure ()
confirmationBox' (Just sendMethod) = do
  elClass "div" "alert alert-success" $ do
    case sendMethod of
      SentWhatsApp -> el "strong" $ text $ i18n EN Sent_WhatsApp
      SentTelegram -> el "strong" $ text $ i18n EN Sent_Telegram
      SentCopy     -> el "strong" $ text $ i18n EN Sent_Copy
      SentRefresh  -> el "strong" $ text $ i18n EN Sent_Refresh
      SentEmail    -> el "strong" $ text $ i18n EN Sent_Email

awesomeAddon :: forall m t. (DomBuilder t m) =>  Text -> m ()
awesomeAddon t =
  elAttr "span" ( "class" =: "input-group-addon") $
    elAttr "i" ("class" =: ("fa " <> t)) blank

copyButton :: forall t m. DomBuilder t m => m (Event t ())
copyButton
  = makeClickable . elAttr' "div" ( "class" =: "input-btn input-btn-right link" <> "title" =: i18n EN Copy_link_to_clipboard 
                                    <> "type" =: "button" <> "role" =: "button"
                                    <> "onClick" =: "copyInvitationLink()"
                                  ) $ blank


refreshLinkButton :: forall t m. DomBuilder t m => m (Event t ())
refreshLinkButton
  = makeClickable . elAttr' "div" ( "class" =: "input-btn input-btn-left recreate" <> "title" =: i18n EN Generate_new_link
                   <> "type" =: "button" <> "role" =: "button") $ blank

showLinkInput :: forall t m. (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
showLinkInput invitationLink =
  let
    makeLinkAttrs link' = ( "type" =: "text"
                            <> "class" =: "mail-input link"
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
emailWidget _ invData = mdo
    req <- elClass "div" "mail-form" $ do
      addrInput <- textInput $ def { _textInputConfig_attributes = (pure $ "placeholder" =: ".."
                                                                       <> "class" =: "mail-input"
                                                                   )
                                   , _textInputConfig_inputType = "email"
                                   }
      let addr = addrInput^.textInput_value
      sendClicked <- sendEmailBtn
      let enterPressed' = enterPressed $ addrInput^.textInput_keypress
      let sendRequested = leftmost [sendClicked, enterPressed']
      -- let infoBox = widgetHold (pure ()) $ showSuccess <$> leftmost [ invSent, const Nothing <$> sendRequested ]
      pure $ buildRequest sendRequested (current addr)
    -- _ <- infoBox'
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

    sendEmailBtn = makeClickable . elAttr' "div" (addBtnAttrs "input-btn mail") $ blank

    -- invSent :: Event t (Maybe API.SendInvitation)
    -- invSent = push (\r -> pure $ case r of
    --                                API.ResSentInvitation inv -> Just (Just inv)
    --                                _ -> Nothing
    --                ) res

    -- showSuccess Nothing = pure ()
    -- showSuccess (Just inv) = do
    --     elAttr "div" ("class" =: "alert alert-success") $
    --       text $ "e-mail successfully sent to: " <> getAddr inv
    --   where
    --     getAddr (API.SendInvitation _ (EmailInvitation addr)) = addr
    --     getAddr _ = "nobody"
