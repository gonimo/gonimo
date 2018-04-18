{-# LANGUAGE GADTs               #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Invite.UI where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (maybe, maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as T
import qualified GHCJS.DOM.Element             as Element
import           Network.HTTP.Types            (urlEncode)
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types       as App
import           Gonimo.Client.Environment     (HasEnvironment)
import           Gonimo.Client.Invite.Internal
import           Gonimo.Client.Invite.UI.I18N
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import qualified Gonimo.Client.Settings        as Settings
import           Gonimo.Client.Util
import           Gonimo.I18N
import qualified Gonimo.SocketAPI              as API
import           Gonimo.SocketAPI.Types        (InvitationId)
import qualified Gonimo.SocketAPI.Types        as API
import           Gonimo.Types                  (InvitationDelivery (..))

ui :: forall model m t. (HasEnvironment model, GonimoM model t m) => App.Loaded t -> Config t -> m (Invite t)
ui loaded config = mdo
    model <- ask
    let baseUrl = getBaseLink model
    (createInvEv, makeCreateInvEv) <- newTriggerEvent
    liftIO $ makeCreateInvEv () -- We want a new invitation every time this widget is rendered
    invite' <- invite model $ config & configCreateInvitation .~ leftmost (createInvEv : (void <$> sentEvents))
    let invite'' = invite' & request %~ (mailReqs <>)

    let currentInvitation = invite''^.invitation
    let invitationLink = maybe "" (makeInvitationLink baseUrl . snd) <$> currentInvitation
    let escapedLink = T.decodeUtf8 . urlEncode True . T.encodeUtf8 <$> invitationLink
    let sentEvents = (SentEmail <$ mailReqs) : reCreateEvents

    linkGotSent <- holdDynUniq False (leftmost [ True  <$ leftmost sentEvents
                                               , False <$ leftmost [ doneClicked
                                                                   , backClicked
                                                                   ]
                                               ]
                                             )
    performEvent_ $ (\goForIt -> when goForIt $ Element.scrollIntoView rawDone True) <$> updated linkGotSent


    backClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank

    el "h1" $ trText Invite_More_Devices
    el "h2" . trDynText $ To_your_family <$> App.currentFamilyName loaded

    confirmationBox $ leftmost sentEvents
    mShare <- runMaybeT shareLink
    invButtons <-
      case mShare of
        Nothing    -> do
          isMobile <- (||) <$> getBrowserProperty "mobile" <*> getBrowserProperty "tablet"
          let onMobile mEv = if isMobile then mEv else pure never
          if isMobile then el "h3" (trText SHARE_INVITATION) else blank
          elClass "div" "invite-buttons" $ do
            whatsAppClicked <- onMobile $ inviteButton "whatsapp" "whatsapp://send?text=" escapedLink
            tgClicked       <- onMobile $ inviteButton "telegram" "tg://msg?text=" escapedLink

            pure [ SentWhatsApp <$ whatsAppClicked
                 , SentTelegram <$ tgClicked
                 ]

        Just share -> do
          el "h3" $ trText SHARE_INVITATION
          elClass "div" "mail-form" $ do
            shareClicked <- shareButton
            performEvent_ $ share
                         <$> tag (current invitationLink) shareClicked
            pure [SentShare <$ shareClicked]

    el "h3" $ trText Email
    mailReqs <- emailWidget (config^.configResponse) currentInvitation

    recreateClicked <- el "div" $ do
      copyClipboardScript
      el "h3" $ trText Invitation_Link
      divClass "mail-form link" $ do
          showLinkInput invitationLink
          clicked <- refreshLinkButton
          copyClicked <- copyButton
          pure [ SentRefresh <$ clicked
               , SentCopy    <$ copyClicked
               ]
    let reCreateEvents = recreateClicked <> invButtons

    el "br" blank
    let doneClass linkGotSent' = if linkGotSent' then "btn-lang next-action" else "btn-lang"
    (doneBtn, _) <- elDynAttr' "div" (addBtnDynAttrs $ doneClass <$> linkGotSent)
                   $ trText Done
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

confirmationBox :: forall model m t. GonimoM model t m
                   => Event t InvitationSent -> m ()
confirmationBox sent = do
  let mSent = Just <$> sent
  closeEvent <- fmap (const Nothing) <$> delay 2 mSent
  let lifeEvent = leftmost [mSent, closeEvent]
  _ <- widgetHold (pure ()) $ confirmationBox' <$> lifeEvent
  pure ()

confirmationBox' :: forall model m t. GonimoM model t m => Maybe InvitationSent -> m ()
confirmationBox' Nothing = pure ()
confirmationBox' (Just SentWhatsApp) = pure () -- We don't know whether we actually sent anything.
confirmationBox' (Just SentTelegram) = pure () -- We don't know whether we actually sent anything.
confirmationBox' (Just sendMethod) = do
  elClass "div" "alert alert-success" $ do
    case sendMethod of
      SentWhatsApp -> el "strong" $ trText Sent_WhatsApp -- Not reached
      SentTelegram -> el "strong" $ trText Sent_Telegram -- Not reached
      SentCopy     -> el "strong" $ trText Sent_Copy
      SentRefresh  -> el "strong" $ trText Sent_Refresh
      SentEmail    -> el "strong" $ trText Sent_Email
      SentShare    -> el "strong" $ trText Sent_Share

awesomeAddon :: forall m t. (DomBuilder t m) =>  Text -> m ()
awesomeAddon t =
  elAttr "span" ( "class" =: "input-group-addon") $
    elAttr "i" ("class" =: ("fa " <> t)) blank

copyButton :: forall model t m. GonimoM model t m => m (Event t ())
copyButton = do
  loc <- view Settings.locale
  let title = i18n <$> loc <*> pure Copy_link_to_clipboard
  let attrs title' = "class"   =: "input-btn input-btn-right link"
                  <> "title"   =: title'
                  <> "type"    =: "button"
                  <> "role"    =: "button"
                  <> "onClick" =: "copyInvitationLink()"
  makeClickable . elDynAttr' "div" (attrs <$> title) $ blank

shareButton :: forall model t m. GonimoM model t m => m (Event t ())
shareButton = do
  loc <- view Settings.locale
  let title = i18n <$> loc <*> pure Share
  let attrs title' = "class"      =: "input-btn mail"
                  <> "title"      =: title'
                  <> "type"       =: "button"
                  <> "role"       =: "button"
                  <> "style"      =: "margin-left:0;"
                  <> "aria-label" =: "share-invitation"

  makeClickable . elDynAttr' "div" (attrs <$> title) $ el "span" $ do
    elClass "i" "fa fa-share-alt" blank
    trText Share

refreshLinkButton :: forall model t m. GonimoM model t m => m (Event t ())
refreshLinkButton = do
  loc <- view Settings.locale
  let title = i18n <$> loc <*> pure Generate_new_link
  let attrs title' = "class" =: "input-btn input-btn-left recreate"
                  <> "title" =: title'
                  <> "type"  =: "button"
                  <> "role"  =: "button"
  makeClickable . elDynAttr' "div" (attrs <$> title) $ blank

showLinkInput :: forall t m. (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
showLinkInput invitationLink =
  let
    makeLinkAttrs link' = "type"     =: "text"
                       <> "class"    =: "mail-input link"
                       <> "readOnly" =: "true"
                       <> "id"       =: "invitationLinkUrlInput"
                       <> "value"    =: link'
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

emailWidget :: forall model t m. GonimoM model t m
  => Event t API.ServerResponse -> Dynamic t (Maybe (InvitationId, API.Invitation))
  -> m (Event t [API.ServerRequest])
emailWidget _ invData = mdo
    req <- elClass "div" "mail-form" $ do
      addrInput <- textInput def { _textInputConfig_attributes =
                                      pure $ "class"       =: "mail-input"
                                          <> "placeholder" =: ".."
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
        makeReqs mId email = maybeToList
          $ API.ReqSendInvitation <$> (API.SendInvitation <$> mId <*> Just (EmailInvitation email))
        mInvId = fmap fst <$> current invData
        invAddr = (,) <$> mInvId <*> addr
      in
        uncurry makeReqs <$> tag invAddr clicked

    sendEmailBtn = makeClickable . elAttr' "div" (addBtnAttrs "input-btn mail") $ trText SEND

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
