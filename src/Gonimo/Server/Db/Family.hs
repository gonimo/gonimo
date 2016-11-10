-- | Functions for working with Family entity
--   This module is intended to be imported qualified as Family
module Gonimo.Server.Db.Family where

import           Control.Monad                   (MonadPlus, guard)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Data.Text                       (Text)

import           Gonimo.Server.Error             (ServerError (NoSuchFamily))

import           Control.Exception               (SomeException)
import           Control.Monad.Freer.Exception   (Exc)
import           Database.Persist.Sql            (SqlBackend)
import           Gonimo.Database.Effects         (Database)
import           Gonimo.Server.Db.Entities       (FamilyId)
import           Gonimo.Server.Db.Entities       (Family (..))
import           Gonimo.Server.Db.Internal       (updateRecord, UpdateT)

type UpdateFamilyT m a = UpdateT Family m a

pushBabyName :: (MonadState Family m, MonadPlus m) => Text -> m ()
pushBabyName name = do
  oldFamily <- get
  let oldBabies = familyLastUsedBabyNames oldFamily
  guard $ not (name `elem` oldBabies)
  put $ oldFamily
    { familyLastUsedBabyNames = take 5 (name : familyLastUsedBabyNames oldFamily)
    }

-- | Update db entity as specified by the given UpdateFamilyT - on Nothing, no update occurs.
update :: FamilyId -> UpdateFamilyT (Eff '[Exc SomeException, Database SqlBackend]) a
       -> MaybeT (Eff '[Exc SomeException, Database SqlBackend]) a
update = updateRecord NoSuchFamily

