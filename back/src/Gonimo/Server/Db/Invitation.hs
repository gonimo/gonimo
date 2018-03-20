{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Family entity
--   This module is intended to be imported qualified as FamilyAccount
module Gonimo.Server.Db.Invitation (insert, get, claim, delete, getBySecret, updateDelivery) where


import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Reader      (ReaderT (..))
import qualified Database.Persist.Class          as Db
import           Database.Persist.Sql            (SqlBackend)
import           Database.Persist                (Entity(..))
import           Data.Bifunctor                  (bimap)
import           Control.Monad.Catch  as X (MonadThrow (..))

import           Gonimo.Database.Effects.Servant as Db
import qualified Gonimo.Db.Entities              as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error
import           Gonimo.SocketAPI.Types          as API
import           Gonimo.Types.Extended           (Secret, InvitationDelivery(..))



-- | Insert a new invitation into the DB.
insert :: MonadIO m => API.Invitation -> ReaderT SqlBackend m API.InvitationId
insert = fmap fromDb . Db.insert . toDb

get :: (MonadThrow m, MonadIO m) => InvitationId -> ReaderT SqlBackend m Invitation
get = fmap fromDb . getErr NoSuchInvitation . toDb

delete :: MonadIO m => InvitationId -> ReaderT SqlBackend m ()
delete = Db.delete . toDb

updateDelivery :: (MonadThrow m, MonadIO m) => InvitationDelivery -> InvitationId -> ReaderT SqlBackend m API.Invitation
updateDelivery d iid' = do
  let iid = toDb iid'
  inv <- getErr NoSuchInvitation iid
  let newInv = inv {
    Db.invitationDelivery = d
  }
  Db.replace iid newInv
  pure $ fromDb newInv

claim :: (MonadThrow m, MonadIO m)
      => AccountId -> Secret -> ReaderT SqlBackend m (API.InvitationId, API.Invitation)
claim aid' secret = do
  let aid = toDb aid'
  (invId, inv) <- getBySecret' secret
  guardWithM InvitationAlreadyClaimed
    $ case Db.invitationReceiverId inv of
      Nothing -> do
        Db.replace invId $ inv { Db.invitationReceiverId = Just aid }
        return True
      Just receiverId' -> return $ receiverId' == aid
  pure (fromDb invId, fromDb inv)

getBySecret :: (MonadThrow m, MonadIO m) => Secret -> ReaderT SqlBackend m (API.InvitationId, API.Invitation)
getBySecret = fmap (bimap fromDb fromDb) . getBySecret'

getBySecret' :: (MonadThrow m, MonadIO m) => Secret -> ReaderT SqlBackend m (Db.InvitationId, Db.Invitation)
getBySecret' secret = do
  Entity invId inv <- Db.getByErr NoSuchInvitation (Db.SecretInvitation secret)
  pure (invId, inv)
