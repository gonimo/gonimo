{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.MessageBox.UI where

import Reflex.Dom.Core
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Client.MessageBox.Internal
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import qualified Gonimo.SocketAPI as API
import Gonimo.Client.Reflex.Dom
import Gonimo.SocketAPI.Types (InvitationReply(..))
import Gonimo.Server.Error (ServerError(..))
import Gonimo.Client.Prelude
import qualified Gonimo.Client.MessageBox.UI.I18N as I18N

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, MonadReader (GonimoEnv t) m)
      => Config t -> m (MessageBox t)
ui config = do
  actions <- fmap switchPromptlyDyn
    . widgetHold (pure never)
    . push (pure . id)
    $ displayMessages <$> config^.configMessage
  pure $ MessageBox actions

displayMessages :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, MonadReader (GonimoEnv t) m)
      => [Message] -> Maybe (m (Event t [Action]))
displayMessages msgs = fmap mconcat <$> (sequence <$> traverse displayMessage msgs)


displayMessage :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, MonadReader (GonimoEnv t) m)
      => Message -> Maybe (m (Event t [Action]))
displayMessage msg = fmap (fmap (:[])) <$> case msg of
  ServerResponse res -> displayResponse res

displayResponse :: forall m t. (DomBuilder t m, MonadReader (GonimoEnv t) m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m)
      => API.ServerResponse -> Maybe (m (Event t Action))
displayResponse msg = case msg of
  API.ResError req err -> displayError req err
  API.ResAnsweredInvitation _ InvitationReject _ -> Just $
    box "Invitation rejected!" "panel-warning" $ do
      trText I18N.The_invitation_got_rejected_and_is_now_invalid
      (never,) <$> delayed 5
  API.ResAnsweredInvitation _ InvitationAccept _ -> Just $
    box "Invitation accepted!" "panel-success" $ do
      trText I18N.Your_device_is_now_a_family_member_make_it_a_baby_station
      (never,) <$> delayed 5
  _ -> Nothing

displayError :: forall m t. (DomBuilder t m, MonadReader (GonimoEnv t) m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m)
      => API.ServerRequest -> ServerError -> Maybe (m (Event t Action))
displayError req err = case (req, err) of
  (_, AlreadyFamilyMember fid) -> Just $
    box "Already a member of this family!" "panel-warning" $ do
      trText I18N.You_are_already_a_member_of_this_family_wanna_switch
      switch' <- buttonAttr ("class" =: "btn btn-block") $ trText I18N.Switch_Family
      (const (SelectFamily fid) <$> switch',) <$> delayed 10
  (_, NoSuchInvitation) -> Just $
    box "Invitation not found!" "panel-danger" $ do
      trText I18N.Invitations_are_only_valid_once
      elClass "span" "hidden-xs" $ trText I18N.Security_and_stuff_you_know
      (never,) <$> delayed 6
  (_, InvitationAlreadyClaimed) -> Just $
    box "Invitation already claimed!" "panel-danger" $ do
      trText I18N.Some_other_device_opened_this_invitation_already
      elClass "span" "hidden-xs" $ trText I18N.Security_and_stuff_you_know
      (never,) <$> delayed 6
  (_,_) -> Nothing

box :: forall m t a. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
      => Text -> Text -> m (Event t a, Event t ()) -> m (Event t a)
box title panelClass inner = mdo
  dynPair <- widgetHold (box' title panelClass inner) $ const (pure (never, never)) <$> closed
  let widgetEvent = switchPromptlyDyn . fmap fst $ dynPair
  let closed = switchPromptlyDyn . fmap snd $ dynPair
  pure widgetEvent

box' :: forall m t a. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
      => Text -> Text -> m (Event t a, Event t ()) -> m (Event t a, Event t ())
box' title panelClass inner = do
  elClass "div" ("panel " <> panelClass) $ do
    closeClicked <- elClass "div" "panel-heading" $
      elAttr "div" ("style" =: "display: flex; justify-content: space-between;") $ do
        el "h1" $ text title
        closeButton
    (ev, closeEvent) <- elClass "div" "panel-body" inner
    pure (ev, leftmost [closeEvent, closeClicked])


closeButton :: DomBuilder t m => m (Event t ())
closeButton = do
  (e, _) <- elClass' "span" "close" $ text "x"
  return $ domEvent Click e
