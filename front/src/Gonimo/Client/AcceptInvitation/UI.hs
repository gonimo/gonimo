{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.AcceptInvitation.UI where

import Reflex.Dom
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId, InvitationId)
import qualified Gonimo.Db.Entities as Db
import Gonimo.Client.AcceptInvitation.Internal
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (maybe, fromMaybe)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (urlEncode)
import Gonimo.Types (InvitationDelivery(..))
import qualified Gonimo.Types as Client
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.SocketAPI.Types as API
import Gonimo.Client.Reflex.Dom
import Gonimo.SocketAPI.Types (InvitationInfo(..))
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Class (lift)

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
      => Config t -> m (AcceptInvitation t)
ui config = fmap (fromMaybe emptyAcceptInvitation) . runMaybeT $ do
    secret <- getInvitationSecret
    -- clearInvitationFromURL
    let claimReq = makeClaimInvitation config secret
    answerReq <- lift . fmap switchPromptlyDyn
                 . widgetHold (pure never)
                 $ ui' config <$> gotInvitation
    pure $ AcceptInvitation (claimReq <> answerReq)
  where
    gotInvitation = push (\res -> case res of
                            API.ResClaimedInvitation _ invInfo -> pure $ Just invInfo
                            _ -> pure Nothing
                        ) (config^.configResponse)

ui' :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace)
      => Config t -> InvitationInfo -> m (Event t [API.ServerRequest])
ui' config invInfo = do
  elClass "div" "panel panel-info" $ do
    elClass "div" "panel-heading" $
      el "h1" $ text "Family Invitation"
    elClass "table" "table" $ do
      el "tbody" $ do
        el "tr" $ do
          el "td" $ text "Family Name:"
          el "td" $ text (Client.familyName . invitationInfoFamily $ invInfo)
        el "tr" $ do
          el "td" $ text "Inviting Device:"
          el "td" $ text (invitationInfoSendingDevice invInfo)
        flip (maybe (pure ())) (invitationInfoSendingUser invInfo) $ \invUser ->
          el "tr" $ do
            el "td" $ text "Inviting User:"
            el "td" $ text invUser
    elClass "div" "panel-body" $ text "Buttons here"
  pure never
