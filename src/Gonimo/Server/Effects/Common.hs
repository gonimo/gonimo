module Gonimo.Server.Effects.Common where


import           Control.Exception.Base                  (SomeException,
                                                          throwIO,
                                                          try)
import           Control.Monad                           ((<=<))
import           Control.Monad.Freer.Exception           (Exc (..))
import           Control.Monad.Freer.Internal            (Eff (..))
import           Control.Monad.Logger                    (Loc, LogLevel,
                                                          LogSource, LogStr)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Reader              (ReaderT)
import           Crypto.Random                           (SystemRandom,
                                                          genBytes, newGenIO)
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool)
import           Database.Persist.Sql                    (SqlBackend,
                                                          runSqlPool)
import           Servant.Subscriber

import qualified Gonimo.Database.Effects                 as Db
import           Gonimo.Database.Effects.PersistDatabase (runExceptionDatabase)
import           Gonimo.Server.State                     (OnlineState)
import           Gonimo.WebAPI                           (GonimoAPI)


type DbPool = Pool SqlBackend
type LoggingFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Config = Config {
  configPool :: !DbPool
, configLog  :: !LoggingFunction
, state      :: !OnlineState
, subscriber :: !(Subscriber GonimoAPI)
}


-- We throw exceptions instead of using Either, because only in this case, are database transactions rolled back.
runDatabaseServer :: forall w . Eff (Exc SomeException ': '[Db.Database SqlBackend]) w
                     -> ReaderT SqlBackend IO w
runDatabaseServer = throwExceptions <=< runExceptionDatabase
  where
    -- Needed to have runSqlPool roll back the transaction:
    throwExceptions :: Either SomeException a -> ReaderT SqlBackend IO a
    throwExceptions  = either (lift . throwIO) return

runDatabaseServerIO :: forall w . Pool SqlBackend
                       -> Eff (Exc SomeException ': '[Db.Database SqlBackend]) w
                       -> IO (Either SomeException w)
runDatabaseServerIO pool = try . flip runSqlPool pool . runDatabaseServer

impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"
