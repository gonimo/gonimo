module Main where

import           Control.Monad.Logger
import qualified Data.ByteString.Char8            as S8
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO                        (Handle, stderr)
import           System.Log.FastLogger            (fromLogStr)
import           Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as Map

import           Gonimo.Server
import qualified Gonimo.Server.State as Server
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects.TestServer
import           Gonimo.WebAPI

logHandle :: Handle
logHandle = stderr

getApp :: Config -> Application
getApp =   serve developmentAPI . getDevelopmentServer

main :: IO ()
main           = do
  pool     <- runLoggingT (createSqlitePool ":memory:" 10) doLogging
  families <- newTVarIO Map.empty
  flip runSqlPool pool $ runMigration migrateAll
  let config   = Config {
    configPool = pool
  , configLog  = logToHandle logHandle
  , state = Server.State families 
  }
  run 8081 . getApp $ config


doLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
doLogging = logToHandle logHandle

logToHandle :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToHandle h loc src level msg = S8.hPutStr h ls
  where ls = logHandleBS loc src level msg

logHandleBS :: Loc -> LogSource -> LogLevel -> LogStr -> S8.ByteString
logHandleBS a b c d = fromLogStr $ defaultLogStr a b c d
