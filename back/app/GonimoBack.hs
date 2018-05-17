{-# LANGUAGE CPP #-}
module Main where

import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Database.Persist.Sqlite
import           Gonimo.Server.Subscriber
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Control.Exception             (Exception)
import           Control.Monad.IO.Class        (MonadIO)
import           Crypto.Random                 (newGenIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Typeable
import           Database.Persist.Postgresql
import           GHC.Generics
import           Gonimo.Db.Entities
import           Gonimo.Server.Config         (Config (..))
import           Gonimo.Server.Error           (fromMaybeErr)
import qualified Gonimo.Server.Messenger       as Messenger
import           Gonimo.Server.NameGenerator   (loadFamilies, loadPredicates)
import           Gonimo.SocketServer           (serve)
import           Network.HTTP.Types.Status
import           Safe                          (readMay)
import           System.Environment            (lookupEnv)

#ifdef DEVELOPMENT
import           System.IO
import           System.Directory
import           System.FilePath (splitFileName)
import           Network.Wai.Middleware.Static
#endif

data StartupError
  = NO_GONIMO_FRONTEND_URL
  | NO_GONIMO_BACK_PORT deriving (Generic, Eq, Show, Typeable)
instance Exception StartupError

runGonimoLoggingT :: MonadIO m => LoggingT m a -> m a
runGonimoLoggingT = runStdoutLoggingT

-- | The logging function used by Gonimo.
getGonimoLogFunc :: IO (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
getGonimoLogFunc = runGonimoLoggingT $ askLoggerIO

#ifdef DEVELOPMENT
devMain :: IO ()
devMain = do
  subscriber' <- atomically $ makeSubscriber
  pool        <- runGonimoLoggingT (createSqlitePool "testdb" 1)
  messenger   <- newTVarIO $ Messenger.empty
  flip runSqlPool pool $ runMigration migrateAll
  names <- loadFamilies
  predicates <- loadPredicates
  generator <- newTVarIO =<< newGenIO
  gonimoLogFunc <- getGonimoLogFunc
  let config = Config {
    _configPool = pool
  , _configMessenger  = messenger
  , _configSubscriber = subscriber'
  , _configNames      = names
  , _configPredicates = predicates
  , _configFrontendURL = "http://localhost:8081/index.html"
  , _configRandom = generator
  , _configLogFunc = gonimoLogFunc
  }
  checkAndFixCurrentDirectory
  run 8081 . addDevServer $ serve config

#else

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
  generator <- newTVarIO =<< newGenIO
  gonimoLogFunc <- getGonimoLogFunc
  let config = Config {
    _configPool = pool
  , _configMessenger  = messenger
  , _configSubscriber = subscriber'
  , _configNames      = names
  , _configPredicates = predicates
  , _configFrontendURL = T.pack frontendURL
  , _configRandom = generator
  , _configLogFunc = gonimoLogFunc
  }
  run port . checkOrigin (T.pack frontendURL) $ serve config

#endif

main :: IO ()
#ifdef DEVELOPMENT
main = devMain
#else
main = prodMain
#endif

#ifdef DEVELOPMENT
addDevServer :: Application -> Application
addDevServer = staticPolicy $ addBase "../front-ghcjs/devRoot" <|> addSlash

-- Yeah this is a hack ...
-- for convenience so we can run gonimo-back both in gonimo and gonimo/back folders.
checkAndFixCurrentDirectory :: IO ()
checkAndFixCurrentDirectory = do
  wd <- getCurrentDirectory
  let (_, fileName) = splitFileName wd
  case fileName of
    "back" -> pure ()
    "gonimo" -> setCurrentDirectory "./back"
    _ -> hPutStrLn stderr "Warning, you have to run gonimo-back from either gonimo or the gonimo/back directory!"

#else
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
#endif
