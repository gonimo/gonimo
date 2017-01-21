{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.MessageBox.UI where

import Reflex.Dom
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId, InvitationId)
import qualified Gonimo.Db.Entities as Db
import Gonimo.Client.MessageBox.Internal
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
import Gonimo.SocketAPI.Types (InvitationReply(..))
import Gonimo.Types (Secret)
import Data.Time.Clock

ui :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
      => Config t -> m (MessageBox t)
ui config = do
  box "Message" "panel-info" (pure ((never :: Event t ()), never))
  pure $ MessageBox

-- displayMessage :: forall m t. (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadIO m, MonadHold t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace, TriggerEvent t m)
--       => Config t -> m (MessageBox t)
-- displayMessage  = case config^.configMessage
--   box "Message" "panel-info" blank
--   pure $ MessageBox

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
    (ev, closeEvent) <- inner
    pure (ev, leftmost [closeEvent, closeClicked])


delayed :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadIO m) => NominalDiffTime -> m (Event t ())
delayed dt = do
  (ev, trigger) <- newTriggerEvent
  liftIO $ trigger ()
  delay dt ev

closeButton :: DomBuilder t m => m (Event t ())
closeButton = do
  (e, _) <- elClass' "span" "close" $ text "x"
  return $ domEvent Click e
