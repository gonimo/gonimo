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
import           Reflex hiding (Request, Response)
import           Generics.Deriving.Base             (Generic)
import           Generics.Deriving.Monoid

import qualified Gonimo.Database.Effects.Servant  as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error              (ServerError)
import           Gonimo.SocketAPI.Model
import qualified Gonimo.Server.Config as Server
import Gonimo.Lib.RequestResponse





data Config r t
  = Config { _serverConfig :: Server.Config
           , _onRequest :: Event t [Request r]
           }

newtype Db r t
  = Db { _onResponse :: Event t (Response r)
       }

-- | Type for database loads.
data ModelDump
  = ModelDump { _dumpedFamilies       :: [(FamilyId, Family)]
              , _dumpedInvitations    :: [(InvitationId, Invitation)]
              , _dumpedAccounts       :: [(AccountId, Account)]
              , _dumpedFamilyAccounts :: [(FamilyAccountId, FamilyAccount)]
              , _dumpedDevices        :: [(DeviceId, Device)]
              } deriving (Generic, Eq)

-- | Commands to be performed by the db.
data Command
  = -- | Make a new family for the given account.
    MakeFamily AccountId
    -- | Create a new invitation for the given family.
    --
    --   The given 'DeviceId' is the inviting device.
  | MakeInvitation DeviceId FamilyId
    -- | Join a family with an account
  | MakeFamilyAccount FamilyId AccountId (Maybe InvitationDelivery)
    -- | Load some data.
  | Load (ReaderT SqlBackend IO ModelDump)
    -- | Generic write to the db, when data is persisted you'll receive a 'Wrote'.
  | Write (ReaderT SqlBackend IO ())

type Request r = RequestResponse r Command

-- | Lens for tetting the command out of a Request.
command :: Lens' (RequestResponse r Command) Command
command = payload

-- | The response to a database 'Command'.
data Result
    -- | Conceptually a 'MadeFamily' result consists of a made family and a
    --   'MadeFamilyAccount' result.
    --
    --   This is because we don't allow families with no members so a new family
    --   will always also have a 'FamilyAccount' entry.
    --
    --   This means a 'MakeFamily' command could also trigger two results, one
    --   alternative MadeFamily only containing the family id and the family and
    --   a 'MadeFamilyAccount'. This would require 'onResponse' to be of type
    --
    -- > Event t [(a, Either ServerError Result)]
    --
    --   instead of the simple
    --
    -- > Event t (a, Either ServerError Result)
    --
    --   so both results can be delivered. At the moment this seems to make
    --   implementations more complex, so we will live with this conceptual
    --   redundance for now.
  = MadeFamily (FamilyId, Family) (FamilyAccountId, FamilyAccount)
  | MadeInvitation InvitationId Invitation
  | MadeFamilyAccount FamilyAccountId FamilyAccount
  | Wrote
  | Loaded ModelDump
  deriving Eq


-- | Either we have a valid 'Result' or an error.
type ErrorResult = Either ServerError Result

type Response r = RequestResponse r ErrorResult

-- | 'Request' constructor.
request :: r -> Command -> Request r
request r c = RequestResponse r c

-- | Lens for accessing the result of the response.
result :: Traversal' (RequestResponse r ErrorResult) Result
result = payload . _Right

errorResult :: Lens' (RequestResponse r ErrorResult) ErrorResult
errorResult = payload

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

-- Instances:

instance Server.HasConfig (Config a t) where
  config = serverConfig

instance Monoid ModelDump where
  mempty = memptydefault
  mappend = mappenddefault


-- Lenses auto generated:

class HasConfig a where
  config :: Lens' (a r t) (Config r t)

  serverConfig :: Lens' (a r t) Server.Config
  serverConfig = config . go
    where
      go :: Lens' (Config r t) Server.Config
      go f config' = (\serverConfig' -> config' { _serverConfig = serverConfig' }) <$> f (_serverConfig config')


  onRequest :: Lens' (a r t) (Event t [Request r])
  onRequest = config . go
    where
      go :: Lens' (Config r t) (Event t [Request r])
      go f config' = (\onRequest' -> config' { _onRequest = onRequest' }) <$> f (_onRequest config')


instance HasConfig Config where
  config = id

class HasDb a where
  db :: Lens' (a r t) (Db r t)

  onResponse :: Lens' (a r t) (Event t (Response r))
  onResponse = db . go
    where
      go :: Lens' (Db r t) (Event t (Response r))
      go f db' = (\onResponse' -> db' { _onResponse = onResponse' }) <$> f (_onResponse db')


instance HasDb Db where
  db = id


class HasModelDump a where
  modelDump :: Lens' a ModelDump

  dumpedFamilies :: Lens' a ([ (FamilyId, Family) ])
  dumpedFamilies = modelDump . go
    where
      go :: Lens' ModelDump ([ (FamilyId, Family) ])
      go f modelDump' = (\dumpedFamilies' -> modelDump' { _dumpedFamilies = dumpedFamilies' }) <$> f (_dumpedFamilies modelDump')


  dumpedInvitations :: Lens' a ([ (InvitationId, Invitation) ])
  dumpedInvitations = modelDump . go
    where
      go :: Lens' ModelDump ([ (InvitationId, Invitation) ])
      go f modelDump' = (\dumpedInvitations' -> modelDump' { _dumpedInvitations = dumpedInvitations' }) <$> f (_dumpedInvitations modelDump')


  dumpedAccounts :: Lens' a ([ (AccountId, Account) ])
  dumpedAccounts = modelDump . go
    where
      go :: Lens' ModelDump ([ (AccountId, Account) ])
      go f modelDump' = (\dumpedAccounts' -> modelDump' { _dumpedAccounts = dumpedAccounts' }) <$> f (_dumpedAccounts modelDump')


  dumpedFamilyAccounts :: Lens' a ([ (FamilyAccountId, FamilyAccount) ])
  dumpedFamilyAccounts = modelDump . go
    where
      go :: Lens' ModelDump ([ (FamilyAccountId, FamilyAccount) ])
      go f modelDump' = (\dumpedFamilyAccounts' -> modelDump' { _dumpedFamilyAccounts = dumpedFamilyAccounts' }) <$> f (_dumpedFamilyAccounts modelDump')


  dumpedDevices :: Lens' a ([ (DeviceId, Device) ])
  dumpedDevices = modelDump . go
    where
      go :: Lens' ModelDump ([ (DeviceId, Device) ])
      go f modelDump' = (\dumpedDevices' -> modelDump' { _dumpedDevices = dumpedDevices' }) <$> f (_dumpedDevices modelDump')


instance HasModelDump ModelDump where
  modelDump = id

