-- | Functions for working with Family entity
module Gonimo.Server.Db.Family where

import           Control.Lens
import           Control.Monad             (MonadPlus, guard, unless, when)
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Monad.Trans.State (StateT (..))
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           Control.Monad.Freer                 (Eff)

import           Gonimo.Server.Error      (ServerError (NoSuchFamily),
                                           ToServerError, toServerError)
import           Gonimo.Server.Types      (DeviceType)
import           Gonimo.Database.Effects  (FullDbConstraint)
import qualified Gonimo.Database.Effects  as Db
import           Gonimo.Database.Effects.Servant     as Db
import           Gonimo.Server.Db.Entities (FamilyId, DeviceId)
import Gonimo.Server.Db.Entities (Family(..))

type UpdateFamilyT m a = StateT Family (MaybeT m) a
type UpdateFamily a = UpdateFamilyT Identity a

pushBabyName :: (MonadState Family m, MonadPlus m) => Text -> m ()
pushBabyName name = do
  oldFamily <- get
  let oldBabies = familyLastUsedBabyNames oldFamily
  guard $ not (name `elem` oldBabies)
  put $ oldFamily
    { familyLastUsedBabyNames = take 5 (name : familyLastUsedBabyNames oldFamily)
    }

updateFamily :: (FullDbConstraint backend Family r) => FamilyId -> UpdateFamilyT (Eff r) a -> Eff r (Maybe a)
updateFamily familyId f = do
  oldFamily <- Db.getErr (NoSuchFamily familyId) familyId
  mr <- runMaybeT . flip runStateT oldFamily $ do
    r <- f
    newFamily <- get
    lift.lift $ Db.replace familyId newFamily
    pure r
  pure $ mr^?_Just._1

