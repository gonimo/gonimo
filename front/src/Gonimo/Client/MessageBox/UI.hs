{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.MessageBox.UI where

import           Control.Lens
import           Data.Text                          (Text)
import           Reflex.Dom.Core

import           Gonimo.Client.MessageBox.Internal
import qualified Gonimo.Client.MessageBox.UI.I18N   as I18N
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Server.Error                (ServerError (..))
import qualified Gonimo.SocketAPI                   as API
import           Gonimo.SocketAPI.Invitation.Legacy (InvitationReply (..))

ui :: forall model m t. GonimoM model t m
      => Config t -> m (MessageBox t)
ui config = do
  actions <- fmap switchPromptlyDyn
    . widgetHold (pure never)
    . push (pure . id)
    $ displayMessages <$> config^.configMessage
  pure $ MessageBox actions

displayMessages :: forall model m t. GonimoM model t m
      => [Message] -> Maybe (m (Event t [Action]))
displayMessages msgs = fmap mconcat <$> (sequence <$> traverse displayMessage msgs)


displayMessage :: forall model m t. GonimoM model t m
      => Message -> Maybe (m (Event t [Action]))
displayMessage msg = fmap (fmap (:[])) <$> case msg of
  ServerResponse res -> displayResponse res

displayResponse :: forall model m t. GonimoM model t m
      => API.ServerResponse -> Maybe (m (Event t Action))
displayResponse msg = case msg of
  API.ResError req err -> displayError req err
  API.ResAnsweredInvitation _ InvitationReject _ -> Just $
    box I18N.Invitation_rejected "panel-warning" $ do
      trText I18N.The_invitation_got_rejected_and_is_now_invalid
      (never,) <$> delayed 5
  API.ResAnsweredInvitation _ InvitationAccept _ -> Just $
    box I18N.Invitation_accepted "panel-success" $ do
      trText I18N.Your_device_is_now_a_family_member_make_it_a_baby_station
      (never,) <$> delayed 5
  _ -> Nothing

displayError :: forall model m t. GonimoM model t m
      => API.ServerRequest -> ServerError -> Maybe (m (Event t Action))
displayError req err = case (req, err) of
  (_, AlreadyFamilyMember fid) -> Just $
    box I18N.Already_a_member_of_this_family "panel-warning" $ do
      trText I18N.You_are_already_a_member_of_this_family_wanna_switch
      switch' <- buttonAttr ("class" =: "btn btn-block") $ trText I18N.Switch_Family
      (const (SelectFamily fid) <$> switch',) <$> delayed 10
  (_, NoSuchInvitation) -> Just $
    box I18N.Invitation_not_found "panel-danger" $ do
      trText I18N.Invitations_are_only_valid_once
      elClass "span" "hidden-xs" $ trText I18N.Security_and_stuff_you_know
      (never,) <$> delayed 6
  (_, InvitationAlreadyClaimed) -> Just $
    box I18N.Invitation_already_claimed "panel-danger" $ do
      trText I18N.Some_other_device_opened_this_invitation_already
      elClass "span" "hidden-xs" $ trText I18N.Security_and_stuff_you_know
      (never,) <$> delayed 6
  (_,_) -> Nothing

box :: forall model m t a. GonimoM model t m
      => I18N.Message -> Text -> m (Event t a, Event t ()) -> m (Event t a)
box title panelClass inner = mdo
  dynPair <- widgetHold (box' title panelClass inner) $ const (pure (never, never)) <$> closed
  let widgetEvent = switchPromptlyDyn . fmap fst $ dynPair
  let closed = switchPromptlyDyn . fmap snd $ dynPair
  pure widgetEvent

box' :: forall model m t a. GonimoM model t m
      => I18N.Message -> Text -> m (Event t a, Event t ()) -> m (Event t a, Event t ())
box' title panelClass inner = do
  elClass "div" "notification overlay" $
    elClass "div" "container" $
      elClass "div" ("panel " <> panelClass) $ do
        closeClicked <- elClass "div" "panel-heading" $
          elAttr "div" ("style" =: "display: flex; justify-content: space-between;") $ do
            el "h1" $ trText title
            closeButton
        (ev, closeEvent) <- elClass "div" "panel-body" inner
        pure (ev, leftmost [closeEvent, closeClicked])


closeButton :: DomBuilder t m => m (Event t ())
closeButton = do
  (e, _) <- elClass' "span" "close" $ text "x"
  return $ domEvent Click e
