{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Backend where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Database.Persist.Sqlite
import           Gonimo.Server.Subscriber
import           Network.Wai
import Control.Monad.IO.Class (liftIO)

import           Control.Monad.IO.Class      (MonadIO)
import           "crypto-api" Crypto.Random  (newGenIO)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
-- import           Database.Persist.Postgresql
import           Gonimo.Db.Entities
import           Gonimo.Server.Effects       (Config (..))
import qualified Gonimo.Server.Messenger     as Messenger
import           Gonimo.Server.NameGenerator (loadFamilies, loadPredicates)
import           Gonimo.SocketServer         (serveSnap)
import           Network.HTTP.Types.Status

-- import           Network.Wai.Middleware.Static
import           Snap.Core                   (Snap)

import           Common.Route
import           Obelisk.Backend
import           Obelisk.Route

runGonimoLoggingT :: MonadIO m => LoggingT m a -> m a
runGonimoLoggingT = runStdoutLoggingT

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- Route handling - websocket route:
      cfg <- getDevConfig
      serve $ serveBackend cfg

  , _backend_routeEncoder = backendRouteEncoder
  }

getDevConfig :: IO Config
getDevConfig = do
  subscriber' <- atomically $ makeSubscriber
  pool        <- runGonimoLoggingT (createSqlitePool "testdb" 1)
  messenger   <- newTVarIO $ Messenger.empty
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates <- loadPredicates
  generator <- newTVarIO =<< newGenIO
  pure $ Config {
      configPool = pool
    , configMessenger  = messenger
    , configSubscriber = subscriber'
    , configNames      = names
    , configPredicates = predicates
    , configFrontendURL = "http://localhost:8000"
    , configRandom = generator
    }

serveBackend :: Config -> R BackendRoute -> Snap ()
serveBackend cfg = \case
  BackendRoute_Api :/ ApiRoute_V1 :/ ()
    -> serveSnap cfg
  BackendRoute_Missing :/ ()
    -> pure ()


checkOrigin :: Text -> Application -> Application
checkOrigin frontendURL app req sendResponse = do
  let
    headers = requestHeaders req
    mOrigin = lookup "Origin" headers
    deny reason = sendResponse $ responseLBS (Status 403 reason) [] mempty
  case mOrigin of
    Nothing -> deny "No Origin Header"
    Just origin -> if T.isPrefixOf (T.decodeUtf8 origin) frontendURL || origin == "file://" -- Support native app!
                      then app req sendResponse
                      else deny "Wrong origin - you nasty boy!"
