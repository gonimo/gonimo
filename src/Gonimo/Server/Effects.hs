{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE UndecidableInstances   #-} -- For MonadBaseControl instance
{-# LANGUAGE TypeFamilies   #-} -- For MonadBaseControl instance
{-# LANGUAGE ScopedTypeVariables   #-} -- For MonadBaseControl instance
{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects (
    MonadServer
  , Server
  , ServerT
  , runServer
  , Config (..)
  , atomically
  , genRandomBytes
  , generateSecret
  , getCurrentTime
  , getState
  , notify
  , updateFamilyRetryEff
  , updateFamilyErrEff
  , updateFamilyEff
  , mayUpdateFamilyEff
  , waitForReaderEff
  , getFamilyEff
  , registerDelay
  , runDb
  , runRandom
  , sendEmail
  -- , timeout
  ) where


import           Control.Concurrent.STM         (STM)
import           Control.Concurrent.STM         (TVar)
import           Control.Exception              (SomeException)
import           Control.Exception.Lifted       (throwIO, catch)
import           Control.Lens
import           Control.Monad.Except           (ExceptT, runExceptT)
import           Control.Monad.Base             (MonadBase, liftBase)
import           Control.Monad.Trans.Control     (MonadBaseControl, liftBaseWith, restoreM, defaultLiftBaseWith, defaultRestoreM, StM, ComposeSt, MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreM)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Error.Class      (throwError, catchError, MonadError)
import           Control.Monad.Trans.Class      (lift, MonadTrans)
import           Control.Monad.Trans.Identity   (runIdentityT, IdentityT)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (StateT (..))
import           Control.Monad.Reader           (ask, MonadReader)
import           Control.Monad.Logger           (MonadLogger, LoggingT)
import           Data.ByteString                (ByteString)
import           Data.Proxy
import           Data.Time.Clock                (UTCTime)
import           Database.Persist.Sql           (SqlBackend)
import           Network.Mail.Mime              (Mail)
import           Servant.Subscriber             (Event, HasLink, IsElem,
                                                 IsSubscribable,
                                                 IsValidEndpoint, MkLink, URI)
import           System.Random                  (StdGen)

import           Gonimo.Server.Db.Entities      (FamilyId)
import           Gonimo.Server.Error            (ServerError (..),
                                                 ToServerError, fromMaybeErr,
                                                 mayThrowLeft, throwServer)
import           Gonimo.Server.State            (lookupFamily,
                                                 updateFamily,
                                                 updateFamilyRetry)
import           Gonimo.Server.State.Types      (FamilyOnlineState,
                                                 OnlineState,
                                                 UpdateFamilyT)
import           Gonimo.Server.Types            (Secret (..))
import           Gonimo.WebAPI                  (GonimoAPI)
import           Utils.Constants                (standardTimeout)
import           Gonimo.Server.State.MessageBox  as MsgBox
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import           Database.Persist.Sql            (SqlBackend, runSqlPool)
import           Data.Pool                               (Pool)
import           Servant.Subscriber              (Subscriber(..))
import qualified Servant.Subscriber              as Subscriber
import qualified Control.Concurrent.STM          as STM
import           Network.Mail.SMTP             (sendMail)
import           Control.Exception.Base        (SomeException, toException, try)
import           Crypto.Random                 (SystemRandom,
                                                newGenIO)
import           Crypto.Classes.Exceptions      (genBytes)
import qualified Data.Time.Clock               as Clock
import           System.Random                           (getStdRandom)
import           Control.Monad.Trans.Control    (MonadTransControl, defaultRestoreT)

secretLength :: Int
secretLength = 16

class (MonadIO m, MonadBaseControl IO m, MonadLogger m) => MonadServer m where
  atomically :: STM a -> m a
  registerDelay :: Int -> m (TVar Bool)
  sendEmail :: Mail -> m ()
  genRandomBytes :: Int -> m ByteString
  getCurrentTime :: m UTCTime
  runDb :: ReaderT SqlBackend IO a -> m a
  runRandom :: (StdGen -> (a, StdGen)) -> m a
  getState :: m OnlineState
  notify :: forall endpoint . (IsElem endpoint GonimoAPI, HasLink endpoint
                              , IsValidEndpoint endpoint, IsSubscribable endpoint GonimoAPI)
            => Event -> Proxy endpoint -> (MkLink endpoint -> URI) -> m ()

type DbPool = Pool SqlBackend

data Config = Config {
  configPool :: !DbPool
, state      :: !OnlineState
, subscriber :: !(Subscriber GonimoAPI)
}



newtype ServerT m a = ServerT (ReaderT Config m a)
                 deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config, MonadTrans)


runServerT :: ServerT m a -> ReaderT Config m a
runServerT (ServerT i) = i

-- newtype Server a = Server (ReaderT Config (LoggingT IO) a)
--                  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config)
type Server = ServerT (LoggingT IO)

runServer :: (forall m. MonadIO m => LoggingT m a -> m a) -> Config -> Server a -> IO a
runServer runLoggingT c = runLoggingT . flip runReaderT c . runServerT

instance (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => MonadServer (ServerT m) where
  atomically = liftIO . STM.atomically
  registerDelay = liftIO . STM.registerDelay
  sendEmail = liftIO . sendMail "localhost"
  genRandomBytes l = fst . genBytes l <$> (liftIO newGenIO :: (ServerT m) SystemRandom)
  getCurrentTime = liftIO Clock.getCurrentTime
  runDb trans = do
    c <- ask
    liftIO $ flip runSqlPool (configPool c) trans
  runRandom rand = liftIO $ getStdRandom rand
  getState = state <$> ask
  notify ev pE f = do
    c <- ask
    liftIO . STM.atomically $ Subscriber.notify (subscriber c) ev pE f

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
  getState = lift getState
  notify ev pE f = lift $ notify ev pE f

generateSecret :: MonadServer m => m Secret
generateSecret = Secret <$> genRandomBytes secretLength


-- | Update family, retrying if updateF returns Nothing
updateFamilyRetryEff :: MonadServer m
                   => ServerError -> FamilyId -> StateT FamilyOnlineState (MaybeT STM) a -> m a
updateFamilyRetryEff err familyId updateF = do
  state <- getState
  timeUp <- registerDelay standardTimeout
  r <- atomically . runMaybeT $ updateFamilyRetry timeUp state familyId updateF
  case r of
    Nothing -> throwServer err
    Just v -> pure v

-- | Update family, throwing any ServerErrors.
updateFamilyErrEff :: (MonadServer m, ToServerError e, Monoid e)
                   => FamilyId -> UpdateFamilyT (ExceptT e STM) a -> MaybeT m a
updateFamilyErrEff familyId updateF = do
  state <- lift getState
  er <- lift . atomically . runExceptT $ updateFamily state familyId updateF
  mayThrowLeft er

-- | May update family if updateF does not return Nothing.
mayUpdateFamilyEff :: MonadServer m
                   => FamilyId -> StateT FamilyOnlineState (MaybeT STM) a -> MaybeT m a
mayUpdateFamilyEff familyId updateF = do
  state <- lift getState
  MaybeT . atomically . runMaybeT $ updateFamily state familyId updateF


-- | Update a family online state.
updateFamilyEff :: MonadServer m
                   => FamilyId -> StateT FamilyOnlineState (IdentityT STM) a -> m a
updateFamilyEff familyId updateF = do
  state <- getState
  atomically . runIdentityT $ updateFamily state familyId updateF


getFamilyEff :: MonadServer m
                =>  FamilyId -> m FamilyOnlineState
getFamilyEff familyId = do
  state <- getState
  mFamily <- atomically $ lookupFamily state familyId
  fromMaybeErr (FamilyNotOnline familyId) mFamily


waitForReaderEff :: forall key val num m. (Ord key, MonadServer m)
                 => ServerError -> FamilyId
                 -> key -> Lens' FamilyOnlineState (MessageBoxes key val num)
                 -> m ()
waitForReaderEff onTimeout familyId key messageBoxes = catch wait handleNotRead
  where
    wait = updateFamilyRetryEff onTimeout familyId
           $ MsgBox.clearIfRead messageBoxes key

    handleNotRead :: SomeException -> m ()
    handleNotRead e = do
      updateFamilyEff familyId $ MsgBox.clearData messageBoxes key
      throwIO e
