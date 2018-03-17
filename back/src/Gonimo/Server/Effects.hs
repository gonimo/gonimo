{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Gonimo.Server.Effects (
    ServerIO
  , unRIO
  , runRIO
  , Config (..)
  , async
  , atomically
  , genRandomBytes
  , generateSecret
  , getCurrentTime
  , getMessenger
  , notify
  , runDb
  , runRandom
  , sendEmail
  , generateFamilyName
  , getPredicatePool
  , getFrontendURL
  , generateInvitationCode
  -- , timeout
  ) where


import qualified Codec.Binary.Base32            as Base32
import           Control.Concurrent.STM         (STM)
import           Control.Concurrent.STM         (TVar)
import qualified Control.Concurrent.STM         as STM

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (MonadLogger(..),
                                                 MonadLoggerIO(..), askLoggerIO,
                                                 Loc, LogSource, LogLevel, LogStr, toLogStr)
import           Control.Monad.Reader           (MonadReader, ask, asks)


import           Control.Monad.Trans.Reader     (ReaderT(..))

import           Data.ByteString                (ByteString)
import           Data.Pool                      (Pool)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time.Clock                (UTCTime)
import           Database.Persist.Sql           (SqlBackend)
import           Database.Persist.Sql           (runSqlPool)
import           Gonimo.Types                   (FamilyName (..),
                                                 InvitationCode (..), Secret (..))
import           Network.Mail.Mime              (Mail)
import           System.Random                  (StdGen)
#ifndef DEVELOPMENT
import           Network.Mail.SMTP              (sendMail)
#endif
import           Control.Concurrent.Async       (Async)
import qualified Control.Concurrent.Async       as Async
import           Control.Concurrent.STM.TVar    (readTVar, writeTVar)

import           Crypto.Classes.Exceptions      (genBytes)
import           Crypto.Random                  (SystemRandom)
import           Data.Text                      (Text)
import qualified Data.Time.Clock                as Clock
import           System.Random                  (getStdRandom)
import           Control.Lens (Getting, view)
import           Control.Monad.Catch  as X (MonadThrow (..))

import           Gonimo.Server.Messenger        (MessengerVar)
import           Gonimo.Server.NameGenerator    (FamilyNames, Predicates)
import qualified Gonimo.Server.NameGenerator    as Gen
import           Gonimo.Server.Subscriber
import           Gonimo.Server.Subscriber.Types
import           Gonimo.SocketAPI               (ServerRequest)

type DbPool = Pool SqlBackend

-- TODO: Create HasConfig - make it polymorph, so we can simply add an additional constraint instead of AuthReader.

data Config = Config {
  _configPool        :: !DbPool
, _configMessenger   :: !MessengerVar
, _configSubscriber  :: !Subscriber
, _configNames       :: !FamilyNames
, _configPredicates  :: !Predicates
, _configFrontendURL :: !Text
, _configRandom      :: !(TVar SystemRandom)
}


$(makeClassy ''Config)

secretLength :: Int
secretLength = 16

-- | Reader IO Monad specialized on IO.
--
--   RIO as in: https://www.fpcomplete.com/blog/2017/07/the-rio-monad
newtype RIO env a = RIO { unRIO :: (ReaderT env IO a) }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow)


-- | Concrete monad stack makes a whole lot of stuff easier.
--
--   See https://www.fpcomplete.com/blog/2017/07/the-rio-monad for details.
--   Modules should use RIO with some polymorphic env for increased modularity and easy testability.
type ServerIO = RIO Config

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

class HasLogFunc env where
  logFuncL :: Getting r env (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog a b c d = do
    f <- view logFuncL
    liftIO $ f a b c $ toLogStr d

instance HasLogFunc env => MonadLoggerIO (RIO env) where
  askLoggerIO = view logFuncL

-- | 'STM.atomically' lifted to the RIO monad.
atomically :: STM a -> RIO env a
atomically = liftIO . STM.atomically

-- | Send an email.
sendEmail :: Mail -> RIO env ()
#ifdef DEVELOPMENT
sendEmail = const $ pure ()
#else
sendEmail = liftIO . sendMail "localhost"
#endif

-- | Convenient wrapper around 'genBytesSecure' in the 'ServerIO' monad.
genRandomBytes :: HasConfig env => Int -> RIO env ByteString
genRandomBytes l = do
    randRef <- view configRandom
    atomically $ genBytesSecure l randRef

-- | Generate some random bytes securely.
-- STM necessary so multiple threads won't return the same secret!
genBytesSecure :: Int -> TVar SystemRandom -> STM ByteString
genBytesSecure l randRef = do
  oldGen <- readTVar randRef
  let (r, newGen) = genBytes l oldGen
  writeTVar randRef newGen -- Probably a problem with threading: https://ghc.haskell.org/trac/ghc/ticket/13751
  pure r


getCurrentTime :: RIO env UTCTime
getCurrentTime = liftIO Clock.getCurrentTime

runDb :: HasConfig env => ReaderT SqlBackend IO a -> RIO env a
runDb trans = do
  c <- ask
  liftIO $ flip runSqlPool (_configPool c) trans

runRandom :: (StdGen -> (a, StdGen)) -> RIO env a
runRandom rand = liftIO $ getStdRandom rand

getMessenger :: HasConfig env => RIO env MessengerVar
getMessenger = view configMessenger

notify :: HasConfig env => ServerRequest -> RIO env ()
notify req = do
  c <- view configSubscriber
  liftIO $ notifyChangeIO c req

async  :: HasConfig env => ServerIO a -> RIO env (Async a)
async task = do
  c <- ask
  let ioTask = runRIO c $ task
  liftIO $ Async.async ioTask

getFamilyNamePool :: HasConfig env => RIO env FamilyNames
getFamilyNamePool = view configNames

getPredicatePool  :: HasConfig env => RIO env Predicates
getPredicatePool = view configPredicates

getFrontendURL :: HasConfig env => RIO env Text
getFrontendURL = asks configFrontendURL


generateSecret :: HasConfig env => RIO env Secret
generateSecret = Secret <$> genRandomBytes secretLength

generateFamilyName :: HasConfig env => RIO env FamilyName
generateFamilyName = do
  preds <- getPredicatePool
  fNames <- getFamilyNamePool
  Gen.generateFamilyName preds fNames


-- | Generate a random invitation code.
--
--   An Invitation code is just Text consisting of 6 characters. (Base32 characters)
generateInvitationCode :: HasConfig env => RIO env InvitationCode
generateInvitationCode = do
    bytes <- genRandomBytes 4
    pure $ makeCode bytes
  where
    makeCode :: ByteString -> InvitationCode
    makeCode = InvitationCode . T.take 6 . T.decodeUtf8 . Base32.encode
