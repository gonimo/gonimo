{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes   #-}
{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects (
    Server
  , ServerConstraint
  , atomically
  , genRandomBytes
  , generateSecret
  , getCurrentTime
  , getState
  , logDebug
  , logError
  , logInfo
  , logMessage
  , logTH
  , logWarn
  , notify
  , updateFamilyRetryEff
  , updateFamilyErrEff
  , updateFamilyEff
  , cleanReceivedEff
  , getFamilyEff
  , registerDelay
  , runDb
  , runRandom
  , sendEmail
  , module Logging
  -- , timeout
  ) where


import           Control.Lens
import           Control.Concurrent.STM        (STM, retry)
import           Control.Concurrent.STM        (TVar)
import           Control.Exception             (SomeException)
import           Control.Monad.Freer           (Eff)
import           Control.Monad.Freer.Exception (Exc)
import           Control.Monad.Logger          (LogLevel (..), LogSource,
                                                ToLogStr, liftLoc)
import           Data.ByteString               (ByteString)
import           Data.Proxy
import           Data.Text
import           Data.Time.Clock               (UTCTime)
import           Database.Persist.Sql          (SqlBackend)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.Mail.Mime             (Mail)
import           Servant.Subscriber            (Event, HasLink, IsElem,
                                                IsSubscribable, IsValidEndpoint,
                                                MkLink, URI)
import           System.Random                  (StdGen)

import           Gonimo.Database.Effects
import           Gonimo.Server.DbEntities       (FamilyId)
import           Gonimo.Server.Effects.Internal
import           Gonimo.Server.Error            (ServerError (NoSuchFamily, InternalServerError),
                                                 fromMaybeErr, throwServer)
import           Gonimo.Server.State            (FamilyOnlineState, OnlineState,
                                                 lookupFamily, updateFamily, UpdateFamily, updateFamilyRetry, cleanReceived, CleanReceivedResult(..), QueueStatus)
import           Gonimo.Server.Types            (Secret (..))
import           Gonimo.WebAPI                  (GonimoAPI)
import           Utils.Constants                (standardDelay)
import           Gonimo.Server.Effects.Logging         as Logging

secretLength :: Int
secretLength = 16


atomically :: ServerConstraint r => STM a -> Eff r a
atomically = sendServer . Atomically

-- timeout :: ServerConstraint r => Int -> ServerEffects a -> Eff r a
-- timeout n eff = sendServer $ Timeout n eff

registerDelay :: ServerConstraint r => Int -> Eff r (TVar Bool)
registerDelay = sendServer . RegisterDelay

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


genRandomBytes :: ServerConstraint r => Int -> Eff r ByteString
genRandomBytes = sendServer . GenRandomBytes

getCurrentTime :: ServerConstraint r => Eff r UTCTime
getCurrentTime = sendServer GetCurrentTime


runDb :: ServerConstraint r => Eff '[Exc SomeException, Database SqlBackend]  a -> Eff r a
runDb = sendServer . RunDb

runRandom :: ServerConstraint r => (StdGen -> (a,StdGen)) -> Eff r a
runRandom = sendServer . RunRandom

generateSecret :: ServerConstraint r => Eff r Secret
generateSecret = Secret <$> genRandomBytes secretLength


getState :: ServerConstraint r => Eff r OnlineState
getState = sendServer GetState

notify :: forall endpoint r. (ServerConstraint r, IsElem endpoint GonimoAPI, HasLink endpoint
                      , IsValidEndpoint endpoint, IsSubscribable endpoint GonimoAPI)
                      => Event -> Proxy endpoint -> (MkLink endpoint -> URI) -> Eff r ()
notify ev pE cb = sendServer $ Notify ev pE cb

-- | Update family, retrying if updateF returns Nothing
updateFamilyRetryEff :: ServerConstraint r
                   => ServerError -> FamilyId -> UpdateFamily a -> Eff r a
updateFamilyRetryEff err familyId updateF = do
  state <- getState
  timeUp <- registerDelay standardDelay
  r <- atomically $ updateFamilyRetry timeUp state familyId updateF
  case r of
    Nothing -> throwServer err
    Just v -> pure v

-- | Update family, throwing err if updateF returns Nothing
updateFamilyErrEff :: ServerConstraint r
                   => ServerError -> FamilyId -> UpdateFamily a -> Eff r a
updateFamilyErrEff err familyId updateF = do
  state <- getState
  r <- atomically $ updateFamily state familyId updateF
  case r of
    Nothing -> throwServer err
    Just v -> return v

-- | Update family, ignoring Nothing.
updateFamilyEff :: ServerConstraint r
                   => FamilyId -> UpdateFamily a -> Eff r (Maybe a)
updateFamilyEff familyId updateF = do
  state <- getState
  atomically $ updateFamily state familyId updateF


getFamilyEff :: ServerConstraint r
                =>  FamilyId -> Eff r FamilyOnlineState
getFamilyEff familyId = do
  state <- getState
  mFamily <- atomically $ lookupFamily state familyId
  fromMaybeErr (NoSuchFamily familyId) mFamily

-- | The given Error is thrown on timeout.
cleanReceivedEff :: ServerConstraint r
                 => ServerError -> FamilyId
                 -> Lens' FamilyOnlineState (Maybe (QueueStatus a))
                 -> Eff r ()
cleanReceivedEff err familyId queue = do
  state <- getState
  timeUp <- registerDelay standardDelay
  r <- atomically $ cleanReceived state familyId timeUp queue
  case r of
    WasReceived -> return ()
    WasNotReceived -> throwServer err
    AlreadyCleaned -> do
      $(logError) $ "Error: message/secret got wiped, but not by us! Damn stupid."
      throwServer InternalServerError
    FamilyNotFoundError -> do
      $(logError) $ "Error: family not found - we just used it - WTF?!"
      throwServer InternalServerError

