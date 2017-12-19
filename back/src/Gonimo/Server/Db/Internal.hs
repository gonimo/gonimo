{-# LANGUAGE TypeFamilies #-}
module Gonimo.Server.Db.Internal where

import           Control.Lens
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (StateT (..))
import           Control.Monad.Base               (MonadBase)
import           Control.Monad.IO.Class           (MonadIO)
import           Database.Persist.Class           (Key, PersistRecordBackend)
import qualified Database.Persist.Class           as Db
import           Database.Persist.Sql             (SqlBackend)
import           Control.Concurrent.STM.TQueue

import qualified Gonimo.Database.Effects.Servant  as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error              (ServerError)

-- | Commands to be performed by the db.
data Command
  = -- | Make a new family for the given account.
    MakeFamily AccountId
    -- | Create a new invitation for the given family.
  | MakeInvitation FamilyId
    -- | Load an invitation by the givne secret.
  | LoadInvitation Secret
    -- | Join a family with an account
  | MakeFamilyAccount FamilyId AccountId (Maybe InvitationDelivery)
    -- | Load all data an account might want to access. (Devices, Families, accounts and devices in those families.)
  | LoadAccount AccountId
    -- | Generic write to the db, no response expected.
  | Write [ReaderT SqlBackend IO ()]

data Request = Request { _requester :: DeviceId -- ^ Used for error reporting.
                       , _command :: Command -- ^ The actual command to execute.
                       }

data Config t
  = Config { _serverConfig :: Server.Config
           , _onRequest :: Event t [Request]
           }

data Db t
  = Db { _onResponse :: Event t Cache.ModelDump
       , _onError :: Event t (Request, ServerError)
       }

type UpdateT entity m a = StateT entity (MaybeT m) a

-- | Update db entity as specified by the given UpdateT - on Nothing, no update occurs.
updateRecord' :: ( PersistRecordBackend record SqlBackend
                , MonadIO m, MonadBase IO m )
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
                , MonadIO m, MonadBase IO m, IsDbType recordId, IsDbType apiRecord, record ~ DbType apiRecord
                , DbType recordId ~ Key record )
                => (recordId -> ServerError) -> recordId
                -> UpdateT apiRecord (ReaderT SqlBackend m) a
                -> MaybeT (ReaderT SqlBackend m) a
updateRecord noSuchRecord recordId f = do
      let recordId' = toDb recordId
      let f' = stateToDb f
      updateRecord' (noSuchRecord recordId) recordId' f'
