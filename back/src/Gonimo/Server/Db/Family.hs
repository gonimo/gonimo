{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Family entity
--   This module is intended to be imported qualified as Family
module Gonimo.Server.Db.Family where

import           Control.Monad              (MonadPlus, guard)
import           Control.Monad.Base         (MonadBase)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.State.Class  as State
import           Control.Monad.Trans.Maybe  (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Maybe                 (listToMaybe)
import           Data.Text                  (Text)
import           Database.Persist.Class     (PersistEntity,
                                             PersistEntityBackend)
import           Database.Persist.Sql       (SqlBackend)
import qualified Database.Persist.Class  as Db
import           Database.Persist           ((==.), Entity(..))
import Control.Arrow

import           Gonimo.Server.Db.Internal  (UpdateT, updateRecord)
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error        (ServerError (NoSuchFamily))
import           Gonimo.SocketAPI.Types
import           Gonimo.Types
import qualified Gonimo.Db.Entities         as Db
import Gonimo.Database.Effects.Servant

type UpdateFamilyT m a = UpdateT Family m a

-- | Create a new family in the database.
insert :: MonadIO m => Family -> ReaderT SqlBackend m FamilyId
insert = fmap fromDb . Db.insert . toDb

get :: (MonadBase IO m, MonadIO m) => FamilyId -> ReaderT SqlBackend m Family
get fid' = fmap fromDb . getErr (NoSuchFamily fid') . toDb $ fid'

deleteMember :: MonadIO m => AccountId -> FamilyId -> ReaderT SqlBackend m ()
deleteMember aid fid = Db.deleteBy $ Db.FamilyMember (toDb aid) (toDb fid)

getAccountIds :: MonadIO m => FamilyId -> ReaderT SqlBackend m [AccountId]
getAccountIds fid = do
  entities <- Db.selectList [ Db.FamilyAccountFamilyId ==. toDb fid ] []
  pure $ map (fromDb . Db.familyAccountAccountId . entityVal) entities

getFamilyAccounts :: MonadIO m => FamilyId -> ReaderT SqlBackend m [FamilyAccount]
getFamilyAccounts fid = do
  entities <- Db.selectList [ Db.FamilyAccountFamilyId ==. toDb fid ] []
  pure $ map (fromDb . entityVal) entities

getInvitations :: MonadIO m => FamilyId -> ReaderT SqlBackend m [(InvitationId, Invitation)]
getInvitations fid = do
  entities <- Db.selectList [ Db.InvitationFamilyId ==. toDb fid ] []
  pure $ (fromDb . entityKey &&& fromDb . entityVal) <$> entities

delete :: MonadIO m => FamilyId -> ReaderT SqlBackend m ()
delete fid' = do
  let fid = toDb fid'
  Db.deleteWhere [ Db.FamilyAccountFamilyId ==. fid ]
  Db.deleteWhere [ Db.InvitationFamilyId ==. fid ]
  Db.delete fid

pushBabyName :: (MonadState Family m, MonadPlus m) => Text -> m ()
pushBabyName name = do
  oldFamily <- State.get
  let oldBabies = familyLastUsedBabyNames oldFamily
  guard $ (Just name /= listToMaybe oldBabies)
  put $ oldFamily
    { familyLastUsedBabyNames = take 5 (name : filter (/= name) oldBabies)
    }

setFamilyName :: (MonadState Family m, MonadPlus m) => Text -> m ()
setFamilyName name = do
  oldFamily <- State.get
  let oldName = familyName oldFamily
  guard $ name /= familyNameName oldName
  put $ oldFamily
    { familyName = oldName { familyNameName = name }
    }

-- | Update db entity as specified by the given UpdateFamilyT - on Nothing, no update occurs.
update :: ( PersistEntity (DbType Family) , MonadIO m, MonadBase IO m
          , PersistEntityBackend (DbType Family) ~ SqlBackend
          )
          => FamilyId -> UpdateFamilyT (ReaderT SqlBackend m) a -> MaybeT (ReaderT SqlBackend m) a
update = updateRecord NoSuchFamily

