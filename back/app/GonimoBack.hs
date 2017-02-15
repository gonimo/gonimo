{-# LANGUAGE CPP #-}
module Main where

import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Static
import           Gonimo.Server.Subscriber

import           Gonimo.SocketServer (serve)
import           Gonimo.Server.Effects             (Config(..))
import           Gonimo.Server.NameGenerator     (loadFamilies, loadPredicates)
import           Database.Persist.Postgresql
import           Control.Monad.IO.Class         (MonadIO)
import           Gonimo.Db.Entities
import qualified Gonimo.Server.Messenger as Messenger
import           Control.Exception             (Exception)
import           GHC.Generics
import qualified Data.Text as T
import           Data.Typeable
import           System.Environment (lookupEnv)
import           Gonimo.Server.Error (fromMaybeErr)
import           Safe (readMay)
import           System.Remote.Monitoring

data StartupError
  = NO_GONIMO_FRONTEND_URL
  | NO_GONIMO_BACK_PORT deriving (Generic, Eq, Show, Typeable)
instance Exception StartupError

runGonimoLoggingT :: MonadIO m => LoggingT m a -> m a
runGonimoLoggingT = runStdoutLoggingT

devMain :: IO ()
devMain = do
  subscriber' <- atomically $ makeSubscriber
  pool        <- runGonimoLoggingT (createSqlitePool "testdb" 1)
  messenger   <- newTVarIO $ Messenger.empty
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates <- loadPredicates
  let config = Config {
    configPool = pool
  , configMessenger  = messenger
  , configSubscriber = subscriber'
  , configNames      = names
  , configPredicates = predicates
  , configFrontendURL = "http://localhost:8081/index.html"
  }
  forkServer "localhost" 9081
  run 8081 $ addDevServer $ serve runGonimoLoggingT config

prodMain :: IO ()
prodMain = do
  frontendURL <- fromMaybeErr NO_GONIMO_FRONTEND_URL =<< lookupEnv "GONIMO_FRONTEND_URL"
  mPort <- lookupEnv "GONIMO_BACK_PORT"
  port <- fromMaybeErr NO_GONIMO_BACK_PORT $ do
      strPort <- mPort
      readMay strPort
  subscriber' <- atomically $ makeSubscriber
  -- empty connection string means settings are fetched from env.
  pool        <- runGonimoLoggingT (createPostgresqlPool "" 10)
  messenger   <- newTVarIO $ Messenger.empty
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates <- loadPredicates
  let config = Config {
    configPool = pool
  , configMessenger  = messenger
  , configSubscriber = subscriber'
  , configNames      = names
  , configPredicates = predicates
  , configFrontendURL = T.pack frontendURL
  }
  forkServer "localhost" (port+1000)
  run port $ serve runGonimoLoggingT config

main :: IO ()
#ifdef DEVELOPMENT
main = devMain
#else
main = prodMain
#endif

addDevServer :: Application -> Application
addDevServer = staticPolicy $ addBase "../front/dist/build/gonimo-front/gonimo-front.jsexe" <|> addSlash
