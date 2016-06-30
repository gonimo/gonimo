module Main where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import qualified Data.ByteString.Char8            as S8
import qualified Data.Map.Strict as Map
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Subscriber
import           System.IO                        (Handle, stderr)
import           System.Log.FastLogger            (fromLogStr)
import           Network.Wai.Middleware.Static

import           Gonimo.Server
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects.TestServer
import qualified Gonimo.Server.State as Server
import           Gonimo.WebAPI

logHandle :: Handle
logHandle = stderr

main :: IO ()
main = do
  let subscriberPath = "subscriber"
  subscriber <- atomically $ makeSubscriber subscriberPath runStderrLoggingT 
  pool       <- runLoggingT (createSqlitePool "testdb" 1) doLogging
  families   <- newTVarIO Map.empty
  flip runSqlPool pool $ runMigration migrateAll
  let config = Config {
    configPool = pool
  , configLog  = logToHandle logHandle
  , state      = Server.State families 
  , subscriber = subscriber
  }
  run 8081 $ serveDevelopment $ serveSubscriber subscriber (getServer config)


serveDevelopment :: Application -> Application
serveDevelopment = staticPolicy $ addBase "../gonimo-front/dist" <|> addSlash


doLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
doLogging = logToHandle logHandle

logToHandle :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToHandle h loc src level msg = S8.hPutStr h ls
  where ls = logHandleBS loc src level msg

logHandleBS :: Loc -> LogSource -> LogLevel -> LogStr -> S8.ByteString
logHandleBS a b c d = fromLogStr $ defaultLogStr a b c d

