{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.AcceptInvitation.UI where

import           Control.Lens
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Maybe               (runMaybeT)
import           Data.Maybe                              (fromMaybe, maybe)
import qualified Data.Set                                as Set
import           Data.Text                               (Text)
import           Reflex.Dom.Core

import           Gonimo.Client.AcceptInvitation.Internal
import           Gonimo.Client.AcceptInvitation.UI.I18N
import           Gonimo.Client.Prelude
import qualified Gonimo.SocketAPI                        as API
import           Gonimo.SocketAPI.Types                  (InvitationInfo (..))
import           Gonimo.SocketAPI.Types                  (InvitationReply (..))
import           Gonimo.Types                            (Secret)
import qualified Gonimo.Types                            as Gonimo


ui :: forall m t. GonimoM t m
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

ui' :: forall m t. GonimoM t m
      => Secret -> InvitationInfo -> m (Event t [API.ServerRequest])
ui' secret invInfo = do
  elClass "div" "notification overlay" $ do
    elClass "div" "container" $
      elClass "div" "panel panel-info" $ do
        elClass "div" "panel-heading" $
          el "h1" $ trText Family_Invitation
        elClass "table" "table" $ do
          el "tbody" $ do
            el "tr" $ do
              el "td" $ trText Family_Name
              el "td" $ text (Gonimo.familyNameName . invitationInfoFamily $ invInfo)
            el "tr" $ do
              el "td" $ trText Inviting_Device
              el "td" $ text (invitationInfoSendingDevice invInfo)
            flip (maybe (pure ())) (invitationInfoSendingUser invInfo) $ \invUser ->
              el "tr" $ do
                el "td" $ trText Inviting_User
                el "td" $ text invUser
        elClass "div" "panel-body" $ do
          elAttr "div" ( "class" =: "btn-group btn-group-justified"
                      <> "role" =: "group"
                      ) $ do
            declined <- groupedButton "btn-danger" $ do
              trText Decline
              elClass "i" "fa fa-fw fa-times" blank
            accepted <- groupedButton "btn-success" $ do
              trText Accept
              elClass "span" "hidden-xs" $ trText This_generous_offer
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
