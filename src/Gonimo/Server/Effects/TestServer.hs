module Gonimo.Server.Effects.TestServer (
  runErrorServer
  , Config(..)
  , ServerEffects) where


import           Control.Exception.Base (try)
import           Control.Monad.Freer.Exception (Exc(..), runError)
import           Control.Monad.Freer.Internal (Eff(..), Arrs, decomp, qApp)
import           Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, ToLogStr(..))
import           Data.Bifunctor (first)
import           Data.Monoid ((<>))
import           Data.Pool (Pool)

import           Database.Persist.Sql (SqlBackend, runSqlPool)
import qualified Gonimo.Database.Effects as Db
import           Gonimo.Database.Effects.PersistDatabase (runExceptionDatabase)
import           Gonimo.Server.Effects.Internal
import           Network.Mail.SMTP (sendMail)
import Control.Monad.Trans.Reader (ReaderT)
import Crypto.Random (SystemRandom)
import Crypto.Random (genBytes)
import Data.Time.Clock (getCurrentTime)

type ServerEffects = Eff '[Exc ServerException, Server]

type DbPool = Pool SqlBackend
type LoggingFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO () 

data Config = Config {
  configPool :: DbPool
  ,  configLog :: LoggingFunction
  , configRandGen :: SystemRandom
  }


runErrorServer :: Config -> Eff (Exc ServerException ': '[Server]) w  -> IO (Either ServerException w)
runErrorServer c = runServer c . runError

runServer :: forall w . Config -> Eff '[Server] (Either ServerException w) -> IO (Either ServerException w)
runServer _ (Val v) = return v
runServer c (E u' q) = case decomp u' of
  Right (SendEmail mail)           -> execIO c q $ sendMail "localhost" mail
  Right (LogMessage loc ls ll msg) -> execIO c q $ doLog loc ls ll (toLogStr msg)
  Right (GenRandomBytes l)         -> let (res, newC) = doGen l
                                      in return res >>= runServer newC . qApp q
  Right (GetCurrentTime)           -> execIO c q getCurrentTime   
                                      
  Right (RunDb trans)              -> runDatabaseServerIO pool trans >>= runServer c . qApp q
  Left  _                          -> error impossibleMessage
  where
    pool = configPool c
    doLog = configLog c
    gen = configRandGen c
    -- runLogger loggerT = runLoggingT loggerT doLog
    doGen l = case genBytes l gen of
      Left e              -> ((Left (RandomGeneratorException e)), c)
      Right (res, newGen) -> (Right res, c { configRandGen = newGen })




runDatabaseServer :: forall w . Eff (Exc Db.DbException ': '[Db.Database SqlBackend]) w
                     -> ReaderT SqlBackend IO (Either ServerException w)
runDatabaseServer = fmap (first dbToServerException) . runExceptionDatabase
  where
    dbToServerException Db.NotFoundException = NotFoundException
    dbToServerException (Db.SystemException e) = SystemException e

runDatabaseServerIO :: forall w . Pool SqlBackend 
                       -> Eff (Exc Db.DbException ': '[Db.Database SqlBackend]) w
                     -> IO (Either ServerException w)
runDatabaseServerIO pool = fmap flattenEither . try . flip runSqlPool pool . runDatabaseServer
  where
    flattenEither (Left e) = Left $ SystemException e
    flattenEither (Right (Left e)) = Left $ e
    flattenEither (Right (Right r)) = Right r
    
execIO :: Config
          -> Arrs '[Server] (Either ServerException b) (Either ServerException w)
          -> IO b
          -> IO (Either ServerException w)
execIO c q action = serverTry action >>= runServer c . qApp q

serverTry :: IO a -> IO (Either ServerException a)
serverTry op = first SystemException <$> try op


impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"
