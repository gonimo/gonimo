{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : Gonimo.Server.Config
Description : Base Config for server operations.
Copyright   : (c) Robert Klotzner, 2018
'Config' provides a basic Reader environment for server operations.
-}
module Gonimo.Server.Config (
    ServerIO
  , RIO
  , unRIO
  , runRIO
  , Config (..)
  , HasConfig (..)
  , LiveCode (..)
  , codeCreated
  , codeCode
  , LiveCodes
  , atomically
  , genRandomBytes
  , genRandomBytesSTM
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
  , forkIO
  , forkIO_
  -- , timeout
  ) where


import qualified Control.Concurrent             as Concurrent
import           Control.Concurrent.STM         (STM)
import           Control.Concurrent.STM         (TVar)
import qualified Control.Concurrent.STM         as STM
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger           (Loc, LogLevel, LogSource,
                                                 LogStr)
import           Control.Monad.Reader           (MonadReader, ask)


import           Control.Monad.Trans.Reader     (ReaderT (..))

import           Data.ByteString                (ByteString)
import           Data.Pool                      (Pool)
import           Data.Time.Clock                (UTCTime)
import           Database.Persist.Sql           (SqlBackend)
import           Database.Persist.Sql           (runSqlPool)
import           Gonimo.Types                   (FamilyName (..),
                                                 Secret (..))
import           Network.Mail.Mime              (Mail)
import           System.Random                  (StdGen)
#ifndef DEVELOPMENT
import           Network.Mail.SMTP              (sendMail)
#endif
import           Control.Concurrent.STM.TVar    (readTVar, writeTVar)
import           Control.Lens                   (makeClassy, makeLenses, view, (^.))
import           Control.Monad.RIO
import           Crypto.Classes.Exceptions      (genBytes)
import           Crypto.Random                  (SystemRandom)
import           Data.Text                      (Text)
import qualified Data.Time.Clock                as Clock
import           System.Random                  (getStdRandom)
import           Data.Map.Strict             (Map)


import           Gonimo.Server.Messenger        (MessengerVar)
import           Gonimo.Server.NameGenerator    (FamilyNames, Predicates)
import qualified Gonimo.Server.NameGenerator    as Gen
import           Gonimo.Server.Subscriber
import           Gonimo.Server.Subscriber.Types
import           Gonimo.SocketAPI               (ServerRequest)
import           Gonimo.SocketAPI.Types         (InvitationCode, InvitationId)
import           Data.IndexedTable           (IndexedTable)

type DbPool = Pool SqlBackend

-- | Code for code based invitations.
data LiveCode = LiveCode {
                           -- | Time stamp when the code was created.
                           _codeCreated :: UTCTime

                           -- | The actual 'InvitationCode'.
                         , _codeCode    :: InvitationCode
                         }
$(makeLenses 'LiveCode)

-- | 'LiveCode' table with lookup based on 'InvitationId' and on 'InvitationCode'.
type LiveCodes = IndexedTable InvitationCode Map InvitationId LiveCode
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

    -- | Currently available codes for accepting invitations.
  , _configLiveCodes       :: TVar LiveCodes
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

-- | RIO version of genRandomBytesSTM
genRandomBytes :: HasConfig env => Int -> RIO env ByteString
genRandomBytes l = do
  env <- ask
  atomically $ genRandomBytesSTM env l

-- | Generate some random bytes securely.
-- STM necessary so multiple threads won't return the same secret!
genRandomBytesSTM :: HasConfig env => env -> Int -> STM ByteString
genRandomBytesSTM env l  = do
  oldGen <- readTVar $ env ^. configRandom
  let (r, newGen) = genBytes l oldGen
  writeTVar (env ^. configRandom) newGen -- Probably a problem with threading: https://ghc.haskell.org/trac/ghc/ticket/13751
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


-- | Compatibility functions, until we have an up2date unliftio package.
forkIO :: RIO env () -> RIO env Concurrent.ThreadId
forkIO t = do
  c <- ask
  liftIO . Concurrent.forkIO $ runRIO c t

-- | Compatibility functions, until we have an up2date unliftio package.
forkIO_ :: RIO env () -> RIO env ()
forkIO_ = void . forkIO
