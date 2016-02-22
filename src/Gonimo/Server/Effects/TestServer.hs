module Gonimo.Server.Effects.TestServer (
  runErrorServer
  , ServerEffects) where


import           Control.Exception.Base (try)
import           Control.Monad.Freer.Exception (Exc(..), runError)
import           Control.Monad.Freer.Internal (Eff(..), Arrs, decomp, qApp)
import           Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import           Data.Bifunctor (first)
import           Data.Monoid ((<>))
import           Data.Pool (Pool)
import qualified Data.Text.IO as T
import           Database.Persist.Sql (SqlBackend, runSqlPool)
import           Gonimo.Server.Effects.Internal
import           Network.Mail.SMTP (sendMail)

type ServerEffects = Eff '[Exc ServerException, Server]

type DbPool = Pool SqlBackend
type LoggingFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO () 

data Config = Config {
  configPool :: DbPool
  ,  configLog :: LoggingFunction
  }


runErrorServer :: Config -> Eff (Exc ServerException ': '[Server]) w  -> IO (Either ServerException w)
runErrorServer c = runServer c . runError

runServer :: forall w . Config -> Eff '[Server] (Either ServerException w) -> IO (Either ServerException w)
runServer _ (Val v) = return v
runServer c (E u' q) = case decomp u' of
  Right (SendEmail mail)         -> execIO q $ sendMail "localhost" mail
  Right (LogMessage loc ls ll msg)     -> execIO q $ doLog loc ls ll msg
  Right (RunDb trans)            -> execIO q $ runSqlPool trans pool
  Left  _                        -> error impossibleMessage
  where
    pool = configPool c
    doLog = configLog c

execIO :: Arrs '[Server] (Either ServerException b) (Either ServerException w)
          -> IO b
          -> IO (Either ServerException w)
execIO q action = serverTry action >>= runServer . qApp q

serverTry :: IO a -> IO (Either ServerException a)
serverTry op = first SystemException <$> try op


impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"
