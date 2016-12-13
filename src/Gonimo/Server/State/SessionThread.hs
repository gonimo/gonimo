-- | Manage automatic session killers
--
module Gonimo.Server.State.SessionThread where


import           Control.Lens
import           Control.Monad             (mzero)
import           Control.Monad.State.Class

import           Gonimo.Server.Db.Entities (DeviceId)
import           Gonimo.Server.State.Types (FamilyOnlineState, sessionThreads)
import           Control.Concurrent.Async  (Async)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)

register :: (MonadState FamilyOnlineState m) => DeviceId -> Async () -> m (Maybe (Async ()))
register deviceId thread = runMaybeT $ do
  old <- use (sessionThreads.at deviceId)
  sessionThreads.at deviceId .= Just thread
  maybe mzero pure $ old

remove :: (MonadState FamilyOnlineState m) => DeviceId -> m (Maybe (Async ()))
remove deviceId = runMaybeT $ do
  old <- use (sessionThreads.at deviceId)
  sessionThreads.at deviceId .= Nothing
  maybe mzero pure $ old
