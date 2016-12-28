{-# LANGUAGE TypeFamilies #-}
module Gonimo.Server.Db.Internal where

import           Control.Lens
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Control.Monad.Trans.State       (StateT (..))

import           Gonimo.Server.Error             (ServerError)

import           Control.Monad.Base              (MonadBase)
import           Control.Monad.IO.Class          (MonadIO)
import           Database.Persist.Class          (Key, PersistStoreWrite, PersistRecordBackend)
import qualified Database.Persist.Class          as Db
import qualified Gonimo.Database.Effects.Servant as Db

type UpdateT entity m a = StateT entity (MaybeT m) a

-- | Update db entity as specified by the given UpdateFamilyT - on Nothing, no update occurs.
updateRecord :: ( PersistStoreWrite backend, PersistRecordBackend record backend
                , MonadIO m, MonadBase IO m)
                => (Key record -> ServerError) -> Key record
                -> UpdateT record (ReaderT backend m) a
                -> MaybeT (ReaderT backend m) a
updateRecord noSuchRecord recordId f = do
  oldRecord <- lift $ Db.getErr (noSuchRecord recordId) recordId
  r <- flip runStateT oldRecord $ do
    r <- f
    newRecord <- get
    lift.lift $ Db.replace recordId newRecord
    pure r
  pure $ r^._1
