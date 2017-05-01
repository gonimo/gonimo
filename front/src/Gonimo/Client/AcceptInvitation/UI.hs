{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.AcceptInvitation.UI where

import           Gonimo.Client.Prelude

import           Control.Lens
import           Control.Monad.Fix                       (MonadFix)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Maybe               (runMaybeT)
import           Data.Maybe                              (fromMaybe, maybe)
import           Data.Text                               (Text)
import           Gonimo.Client.AcceptInvitation.Internal
import qualified Gonimo.SocketAPI                        as API
import           Gonimo.SocketAPI.Types                  (InvitationInfo (..))
import           Gonimo.SocketAPI.Types                  (InvitationReply (..))
import           Gonimo.Types                            (Secret)
import qualified Gonimo.Types                            as Gonimo
import           Reflex.Dom.Core
import           GHCJS.DOM.Types (MonadJSM)
import qualified Data.Set                                as Set
import           Gonimo.Client.AcceptInvitation.UI.I18N
import           Gonimo.I18N

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
      => Config t -> m (AcceptInvitation t)
ui config = fmap (fromMaybe emptyAcceptInvitation) . runMaybeT $ do
    secret <- getInvitationSecret
    clearInvitationFromURL
    answerReq <- lift . fmap switchPromptlyDyn -- Only display until user accepted/declined.
                 . widgetHold (onInvitationUI secret)
                 $ const (pure never) <$> gotAnswerResponse

    -- Make sure invitation gets claimed, by subscribing the request:
    claimSub <- lift . holdDyn (Set.singleton (API.ReqClaimInvitation secret)) $ const Set.empty <$> answerReq
    pure $ AcceptInvitation answerReq claimSub
  where
    onInvitationUI secret = fmap switchPromptlyDyn
                     . widgetHold (pure never)
                     $ ui' secret <$> gotInvitation
    gotInvitation = push (\res -> case res of
                            API.ResClaimedInvitation _ invInfo -> pure $ Just invInfo
                            _ -> pure Nothing
                        ) (config^.configResponse)
    gotAnswerResponse = push (\res -> case res of
                            API.ResAnsweredInvitation _ _ _ -> pure $ Just ()
                            API.ResError (API.ReqAnswerInvitation _ _) _ -> pure $ Just ()
                            _ -> pure Nothing
                        ) (config^.configResponse)

ui' :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace)
      => Secret -> InvitationInfo -> m (Event t [API.ServerRequest])
ui' secret invInfo = do
  elClass "div" "fullScreenOverlay" $ do
    elClass "div" "panel panel-info" $ do
      elClass "div" "panel-heading" $
        el "h1" $ text $ i18n EN_GB Family_Invitation
      elClass "table" "table" $ do
        el "tbody" $ do
          el "tr" $ do
            el "td" $ text $ i18n EN_GB Family_Name
            el "td" $ text (Gonimo.familyName . invitationInfoFamily $ invInfo)
          el "tr" $ do
            el "td" $ text $ i18n EN_GB Inviting_Device
            el "td" $ text (invitationInfoSendingDevice invInfo)
          flip (maybe (pure ())) (invitationInfoSendingUser invInfo) $ \invUser ->
            el "tr" $ do
              el "td" $ text $ i18n EN_GB Inviting_User
              el "td" $ text invUser
      elClass "div" "panel-body" $ do
        elAttr "div" ( "class" =: "btn-group btn-group-justified"
                    <> "role" =: "group"
                    ) $ do
          declined <- groupedButton "btn-danger" $ do
            text $ i18n EN_GB Decline
            elClass "i" "fa fa-fw fa-times" blank
          accepted <- groupedButton "btn-success" $ do
            text $ i18n EN_GB Accept
            elClass "span" "hidden-xs" $ text $ i18n EN_GB This_generous_offer
            elClass "i" "fa fa-fw fa-check" blank
          pure $ mconcat [ makeAnswerInvitation secret . fmap (const InvitationReject) $ declined
                         , makeAnswerInvitation secret . fmap (const InvitationAccept) $ accepted
                         ]

groupedButton :: DomBuilder t m => Text -> m () -> m (Event t ())
groupedButton className inner = do
  (e, _) <- elAttr "div" ("class" =: "btn-group" <> "role" =: "group") $
    elAttr' "button" ( "class" =: ("btn btn-block " <> className)
                       <> "type" =: "button"
                     ) inner
  return $ domEvent Click e
