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
  }
  run 8081 $ addDevServer $ serve runGonimoLoggingT config

prodMain :: IO ()
prodMain = do
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
  }
  run 8081 $ serve runGonimoLoggingT config

main :: IO ()
#ifdef DEVELOPMENT
main = devMain
#else
main = prodMain
#endif

addDevServer :: Application -> Application
addDevServer = staticPolicy $ addBase "../front/dist/build/gonimo-front/gonimo-front.jsexe" <|> addSlash
