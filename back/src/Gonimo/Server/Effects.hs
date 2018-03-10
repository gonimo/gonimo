{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-} -- For MonadBaseControl instance
{-# LANGUAGE UndecidableInstances  #-} -- For MonadBaseControl instance
module Gonimo.Server.Effects (
    MonadServer
  , Server
  , ServerT
  , runServer
  , Config (..)
  , async
  , async_
  , atomically
  , genRandomBytes
  , generateSecret
  , getCurrentTime
  , getMessenger
  , notify
  , registerDelay
  , runDb
  , runRandom
  , sendEmail
  , generateFamilyName
  , getPredicatePool
  , getFrontendURL
  -- , timeout
  ) where


import           Control.Concurrent.Async       (Async)
import qualified Control.Concurrent.Async       as Async
import           Control.Concurrent.STM         (STM, TVar)
import qualified Control.Concurrent.STM         as STM
import           Control.Concurrent.STM.TVar    (readTVar, writeTVar)
import           Control.Monad.Base             (MonadBase, liftBase)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (LoggingT, MonadLogger,
                                                 MonadLoggerIO, askLoggerIO,
                                                 runLoggingT)
import           Control.Monad.Reader           (MonadReader, ask, asks)
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import           Control.Monad.Trans.Control    (ComposeSt, MonadBaseControl,
                                                 MonadTransControl, StM, StT,
                                                 defaultLiftBaseWith,
                                                 defaultLiftWith,
                                                 defaultRestoreM,
                                                 defaultRestoreM, liftBaseWith,
                                                 liftWith, restoreM, restoreT,
                                                 defaultRestoreT)
import           Control.Monad.Trans.Maybe      (MaybeT)
import           Control.Monad.Trans.Reader     (ReaderT, runReaderT)
import           Control.Monad.Trans.State      (StateT (..))
import           Crypto.Classes.Exceptions      (genBytes)
import           Crypto.Random                  (SystemRandom)
import           Data.ByteString                (ByteString)
import           Data.Pool                      (Pool)
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime)
import qualified Data.Time.Clock                as Clock
import           Database.Persist.Sql           (SqlBackend,runSqlPool)
import           Network.Mail.Mime              (Mail)
#ifndef DEVELOPMENT
import           Network.Mail.SMTP              (sendMail)
#endif
import           System.Random                  (StdGen, getStdRandom)

import           Gonimo.Server.Messenger        (MessengerVar)
import           Gonimo.Server.NameGenerator    (FamilyNames, Predicates)
import           Gonimo.Server.Subscriber
import           Gonimo.Server.Subscriber.Types
import           Gonimo.SocketAPI               (ServerRequest)
import           Gonimo.Types                   (FamilyName (..), Secret (..))
import qualified Gonimo.Server.NameGenerator    as Gen


secretLength :: Int
secretLength = 16

class (MonadIO m, MonadBaseControl IO m, MonadLoggerIO m) => MonadServer m where
  atomically :: STM a -> m a
  registerDelay :: Int -> m (TVar Bool)
  sendEmail :: Mail -> m ()
  genRandomBytes :: Int -> m ByteString
  getCurrentTime :: m UTCTime
  runDb :: ReaderT SqlBackend IO a -> m a
  runRandom :: (StdGen -> (a, StdGen)) -> m a
  getMessenger :: m MessengerVar
  notify :: ServerRequest -> m ()
  async  :: Server a -> m (Async a)
  getFamilyNamePool :: m FamilyNames
  getPredicatePool  :: m Predicates
  getFrontendURL :: m Text

-- | Ignore Async result - yeah forkIO would work as well.
async_ :: MonadServer m => Server () -> m ()
async_ action = do
  _ :: Async () <- async action
  pure ()

type DbPool = Pool SqlBackend

data Config = Config {
  configPool       :: !DbPool
, configMessenger  :: !MessengerVar
, configSubscriber :: !Subscriber
, configNames      :: !FamilyNames
, configPredicates :: !Predicates
, configFrontendURL :: !Text
, configRandom      :: !(TVar SystemRandom)
}

newtype ServerT m a = ServerT (ReaderT Config m a)
                 deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadReader Config, MonadTrans)


runServerT :: ServerT m a -> ReaderT Config m a
runServerT (ServerT i) = i

-- newtype Server a = Server (ReaderT Config (LoggingT IO) a)
--                  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config)
type Server = ServerT (LoggingT IO)

runServer :: (forall m. MonadIO m => LoggingT m a -> m a) -> Config -> Server a -> IO a
runServer runLogging c = runLogging . flip runReaderT c . runServerT

instance (MonadIO m, MonadBaseControl IO m, MonadLoggerIO m)
  => MonadServer (ServerT m) where
  atomically = liftIO . STM.atomically
  registerDelay = liftIO . STM.registerDelay

#ifdef DEVELOPMENT
  sendEmail = const $ pure ()
#else
  sendEmail = liftIO . sendMail "localhost"
#endif
  genRandomBytes l = do
      randRef <- asks configRandom
      genBytesSecure randRef
    where
      -- STM necessary so multiple threads won't return the same secret!
      genBytesSecure randRef = atomically $ do
        oldGen <- readTVar randRef
        let (r, newGen) = genBytes l oldGen
        writeTVar randRef (seq newGen newGen) -- seq hopefully prevents loop ghc bug. https://ghc.haskell.org/trac/ghc/ticket/13751
        pure r

  getCurrentTime = liftIO Clock.getCurrentTime
  runDb trans = do
    pool <- asks configPool
    liftIO $ runSqlPool trans pool
  runRandom rand = liftIO $ getStdRandom rand
  getMessenger = configMessenger <$> ask
  notify req = do
    subs <- asks configSubscriber
    liftIO $ notifyChangeIO subs req
  async (ServerT task) = do
    c <- ask
    logFunc <- askLoggerIO
    let ioTask = runLoggingT (runReaderT task c) logFunc
    liftIO $ Async.async ioTask
  getFamilyNamePool = asks configNames
  getPredicatePool  = asks configPredicates
  getFrontendURL    = asks configFrontendURL

instance MonadIO m => MonadBase IO (ServerT m) where
  liftBase = liftIO

instance (MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (ServerT m) where
  type StM (ServerT m) a = ComposeSt ServerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadTransControl ServerT where
    type StT ServerT a = StT (ReaderT Config) a
    liftWith = defaultLiftWith ServerT runServerT
    restoreT = defaultRestoreT ServerT

instance MonadServer m => MonadServer (ReaderT c m) where
  atomically = lift . atomically
  registerDelay = lift . registerDelay
  sendEmail = lift . sendEmail
  genRandomBytes = lift . genRandomBytes
  getCurrentTime = lift getCurrentTime
  runDb = lift . runDb
  runRandom = lift . runRandom
  getMessenger = lift getMessenger
  notify = lift . notify
  async = lift . async
  getFamilyNamePool = lift getFamilyNamePool
  getPredicatePool  = lift getPredicatePool
  getFrontendURL  = lift getFrontendURL

instance MonadServer m => MonadServer (StateT c m) where
  atomically = lift . atomically
  registerDelay = lift . registerDelay
  sendEmail = lift . sendEmail
  genRandomBytes = lift . genRandomBytes
  getCurrentTime = lift getCurrentTime
  runDb = lift . runDb
  runRandom = lift . runRandom
  getMessenger = lift getMessenger
  notify = lift . notify
  async = lift . async
  getFamilyNamePool = lift getFamilyNamePool
  getPredicatePool  = lift getPredicatePool
  getFrontendURL  = lift getFrontendURL

instance MonadServer m => MonadServer (MaybeT m) where
  atomically = lift . atomically
  registerDelay = lift . registerDelay
  sendEmail = lift . sendEmail
  genRandomBytes = lift . genRandomBytes
  getCurrentTime = lift getCurrentTime
  runDb = lift . runDb
  runRandom = lift . runRandom
  getMessenger = lift getMessenger
  notify = lift . notify
  async = lift . async
  getFamilyNamePool = lift getFamilyNamePool
  getPredicatePool  = lift getPredicatePool
  getFrontendURL  = lift getFrontendURL

generateSecret :: MonadServer m => m Secret
generateSecret = Secret <$> genRandomBytes secretLength

generateFamilyName :: MonadServer m => m FamilyName
generateFamilyName = do
  preds <- getPredicatePool
  fNames <- getFamilyNamePool
  Gen.generateFamilyName preds fNames
