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
  , RIO
  , unRIO
  , runRIO
  , Config (..)
  , HasConfig (..)
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

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger           (Loc, LogSource, LogLevel, LogStr)
import           Control.Monad.Reader           (MonadReader, ask)


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
import           Control.Concurrent.STM.TVar    (readTVar, writeTVar)

import           Crypto.Classes.Exceptions      (genBytes)
import           Crypto.Random                  (SystemRandom)
import           Data.Text                      (Text)
import qualified Data.Time.Clock                as Clock
import           System.Random                  (getStdRandom)
import           Control.Lens (view, makeClassy, (^.))
import           Control.Monad.RIO

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
  , _configLogFunc     :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }


$(makeClassy ''Config)

instance HasLogFunc Config where
  logFuncL = configLogFunc

secretLength :: Int
secretLength = 16


-- | Concrete monad stack makes a whole lot of stuff easier.
--
--   See https://www.fpcomplete.com/blog/2017/07/the-rio-monad for details.
--   Modules should use RIO with some polymorphic env for increased modularity and easy testability.
type ServerIO = RIO Config



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
  liftIO $ flip runSqlPool (c ^. configPool) trans

runRandom :: (StdGen -> (a, StdGen)) -> RIO env a
runRandom rand = liftIO $ getStdRandom rand

getMessenger :: (MonadReader env m, HasConfig env) => m MessengerVar
getMessenger = view configMessenger

notify :: HasConfig env => ServerRequest -> RIO env ()
notify req = do
  c <- view configSubscriber
  liftIO $ notifyChangeIO c req


getFamilyNamePool :: HasConfig env => RIO env FamilyNames
getFamilyNamePool = view configNames

getPredicatePool  :: HasConfig env => RIO env Predicates
getPredicatePool = view configPredicates

getFrontendURL :: HasConfig env => RIO env Text
getFrontendURL = view configFrontendURL


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
