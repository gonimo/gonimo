{-# LANGUAGE TypeFamilies #-}
module Gonimo.Server.Db.Internal where

import           Control.Lens
import           Control.Monad.Catch              as X (MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (StateT (..))
import           Database.Persist.Class           (Key, PersistRecordBackend)
import qualified Database.Persist.Class           as Db
import           Database.Persist.Sql             (SqlBackend)

import qualified Gonimo.Database.Effects.Servant  as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error              (ServerError)

type UpdateT entity m a = StateT entity (MaybeT m) a

-- | Update db entity as specified by the given UpdateT - on Nothing, no update occurs.
updateRecord' :: ( PersistRecordBackend record SqlBackend
                , MonadIO m, MonadThrow m )
                => ServerError -> Key record
                -> UpdateT record (ReaderT SqlBackend m) a
                -> MaybeT (ReaderT SqlBackend m) a
updateRecord' noSuchRecord recordId f = do
  oldRecord <- lift $ Db.getErr noSuchRecord recordId
  r <- flip runStateT oldRecord $ do
    r <- f
    newRecord <- get
    lift.lift $ Db.replace recordId newRecord
    pure r
  pure $ r^._1

updateRecord :: ( PersistRecordBackend record SqlBackend
                , MonadIO m, MonadThrow m, IsDbType recordId, IsDbType apiRecord, record ~ DbType apiRecord
                , DbType recordId ~ Key record )
                => (recordId -> ServerError) -> recordId
                -> UpdateT apiRecord (ReaderT SqlBackend m) a
                -> MaybeT (ReaderT SqlBackend m) a
updateRecord noSuchRecord recordId f = do
      let recordId' = toDb recordId
      let f' = stateToDb f
      updateRecord' (noSuchRecord recordId) recordId' f'
