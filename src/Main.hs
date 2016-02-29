module Main where

import Data.Text
import Gonimo.Server
import Gonimo.Server.Effects.TestServer
import Gonimo.WebAPI
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.Persist.Sqlite
import Crypto.Random
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as S8
import System.IO (Handle, stderr)
import System.Log.FastLogger (fromLogStr)

logHandle :: Handle
logHandle = stderr

getApp :: Config -> Application
getApp =   serve gonimoAPI . getServer

main :: IO ()
main = do
  sysGen <- newGenIO
  pool <- runLoggingT (createSqlitePool ":memory" 10) doLogging
  let config = Config {
  configPool = pool
    , configLog = logToHandle logHandle
    , configRandGen = sysGen
    }
  run 8081 . getApp $ config


doLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
doLogging = logToHandle logHandle

logToHandle :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
logToHandle h loc src level msg =
    S8.hPutStr h ls
  where
    ls = logHandleBS loc src level msg

logHandleBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> S8.ByteString
logHandleBS a b c d =
    fromLogStr $ defaultLogStr a b c d
