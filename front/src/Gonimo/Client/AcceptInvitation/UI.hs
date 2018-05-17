{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.AcceptInvitation.UI where

import           Control.Lens


import qualified Data.Map                               as Map

import           Data.Text                              (Text)
import           Reflex.Dom.Core

import           Gonimo.Client.AcceptInvitation.UI.I18N
import           Gonimo.Client.Account              (ClaimedInvitations,
                                                         HasAccount (..),
                                                         HasConfig (..))
import qualified Gonimo.Client.Account              as Account
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import           Gonimo.SocketAPI.Invitation.Legacy     (InvitationInfo (..),
                                                         InvitationReply (..), InvitationSecret)
import qualified Gonimo.Types                           as Gonimo




type HasModelConfig c t = (IsConfig c t, Account.HasConfig c)

type HasModel model = HasAccount model

ui :: forall m conf model t
      . (GonimoM model t m, HasModel model, HasModelConfig conf t)
      => model t -> m (conf t)
ui = networkViewFlatten . fmap renderNextInvitation . view Account.claimedInvitations
  where
    nextInvitation :: ClaimedInvitations -> Maybe (InvitationSecret, InvitationInfo)
    nextInvitation = listToMaybe . Map.toList

    renderNextInvitation :: ClaimedInvitations -> m (conf t)
    renderNextInvitation = maybe (pure mempty) (uncurry ui') . nextInvitation


ui' :: forall conf model m t. (GonimoM model t m, HasModelConfig conf t)
      => InvitationSecret -> InvitationInfo -> m (conf t)
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
            pure $ mempty & onAnswerInvitation .~ leftmostList [ (secret, InvitationReject) <$ declined
                                                               , (secret, InvitationAccept) <$ accepted
                                                               ]

groupedButton :: DomBuilder t m => Text -> m () -> m (Event t ())
groupedButton className inner = do
  (e, _) <- elAttr "div" ("class" =: "btn-group" <> "role" =: "group") $
    elAttr' "button" ( "class" =: ("btn btn-block " <> className)
                       <> "type" =: "button"
                     ) inner
  return $ domEvent Click e
