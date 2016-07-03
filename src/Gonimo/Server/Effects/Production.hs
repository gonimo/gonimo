module Gonimo.Server.Effects.Production (
  runExceptionServer
  , Config(..)
  , ServerEffects ) where


import           Control.Exception.Base                  (SomeException,
                                                          throwIO, toException,
                                                          try)
import           Control.Monad.Freer.Exception           (Exc (..), runError)
import           Control.Monad.Freer.Internal            (Arrs, Eff (..),
                                                          decomp, qApp)
import           Control.Monad.Logger                    (Loc, LogLevel,
                                                          LogSource, LogStr,
                                                          ToLogStr (..))
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool)
import           Control.Monad                           ((<=<))
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Reader              (ReaderT)
import           Crypto.Random                           (SystemRandom,
                                                          genBytes, newGenIO)
import           Data.Bifunctor
import           Data.Time.Clock                         (getCurrentTime)
import           Database.Persist.Sql                    (SqlBackend,
                                                          runSqlPool)
import           Network.Mail.SMTP                       (sendMail)
import           Servant.Subscriber
import           Control.Concurrent.STM (atomically)

import qualified Gonimo.Database.Effects                 as Db
import           Gonimo.Database.Effects.PersistDatabase (runExceptionDatabase)
import           Gonimo.Server.Effects.Internal
import qualified Gonimo.Server.State as Server
import           Gonimo.WebAPI (GonimoAPI)
import Gonimo.Server.Effects.Common


runExceptionServer :: Config -> Eff (Exc SomeException ': '[Server]) w  -> IO (Either SomeException w)
runExceptionServer c = runServer c . runError



runServer :: forall w . Config -> Eff '[Server] (Either SomeException w) -> IO (Either SomeException w)
runServer _ (Val v) = return v
runServer c (E u' q) = case decomp u' of
  Right (Atomically m)             -> execIO c q $ atomically m
  Right (SendEmail mail)           -> execIO c q $ sendMail "localhost" mail
  Right (LogMessage loc ls ll msg) -> execIO c q $ doLog loc ls ll (toLogStr msg)
  Right (GenRandomBytes l)         ->
    -- Throw away the new generator & make an exception of any occurred error:
    bimap toException fst . genBytes l <$> (newGenIO :: IO SystemRandom)
    >>= runServer c . qApp q

  Right GetCurrentTime             -> execIO c q getCurrentTime
  Right GetState                   -> runServer c . qApp q $ Right (state c)
  Right (Notify ev pE cB)          -> execIO c q $ atomically (notify (subscriber c) ev pE cB)
  Right (RunDb trans)              -> runDatabaseServerIO pool trans >>= runServer c . qApp q
  Left  _                          -> error impossibleMessage
  where
    pool = configPool c
    doLog = configLog c
    -- runLogger loggerT = runLoggingT loggerT doLog


execIO :: Config
          -> Arrs '[Server] (Either SomeException b) (Either SomeException w)
          -> IO b
          -> IO (Either SomeException w)
execIO c q action = try action >>= runServer c . qApp q
