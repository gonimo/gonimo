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
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Network.HTTP.Types.Status
import           Crypto.Random                 (newGenIO)


runGonimoLoggingT :: MonadIO m => LoggingT m a -> m a
runGonimoLoggingT = runStdoutLoggingT

config :: IO Config
config = do
  pool        <- runGonimoLoggingT (createSqlitePool "testdb" 1)
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates' <- loadPredicates
  generator <- newTVarIO =<< newGenIO
  pure $  Config {
                   _dbPool = pool
                 , _familyNames      = names
                 , _predicates = predicates'
                 , configFrontendURL = "http://localhost:8081/index.html"
                 , _random = generator
                 }

main :: IO ()
main = run 8081 . addDevServer . serve =<< config

addDevServer :: Application -> Application
addDevServer = staticPolicy $ addBase "../front/devRoot" <|> addSlash
