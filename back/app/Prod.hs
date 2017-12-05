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

data StartupError
  = NO_GONIMO_FRONTEND_URL
  | NO_GONIMO_BACK_PORT deriving (Generic, Eq, Show, Typeable)
instance Exception StartupError

runGonimoLoggingT :: MonadIO m => LoggingT m a -> m a
runGonimoLoggingT = runStdoutLoggingT

config :: IO Config
config = do
  frontendURL <- fromMaybeErr NO_GONIMO_FRONTEND_URL =<< lookupEnv "GONIMO_FRONTEND_URL"
  mPort <- lookupEnv "GONIMO_BACK_PORT"
  port <- fromMaybeErr NO_GONIMO_BACK_PORT $ do
      strPort <- mPort
      readMay strPort
  -- empty connection string means settings are fetched from env.
  pool        <- runGonimoLoggingT (createPostgresqlPool "" 10)
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates' <- loadPredicates
  generator <- newTVarIO =<< newGenIO
  pure $ Config { _dbPool = pool
                , _familynames      = names
                , _predicates = predicates'
                , configFrontendURL = T.pack frontendURL
                , _random = generator
                }

main :: IO ()
main = run port . checkOrigin (T.pack frontendURL) . serve =<< config

checkOrigin :: Text -> Application -> Application
checkOrigin frontendURL app req sendResponse = do
  let
    headers = requestHeaders req
    mOrigin = lookup "Origin" headers
    deny reason = sendResponse $ responseLBS (Status 403 reason) [] mempty
  case mOrigin of
    Nothing -> deny "No Origin Header"
    Just origin -> if T.isPrefixOf (T.decodeUtf8 origin) frontendURL
                      then app req sendResponse
                      else deny "Wrong origin - you nasty boy!"
