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
  , module Logging
  -- , timeout
  ) where


import           Control.Concurrent.STM         (STM)
import           Control.Concurrent.STM         (TVar)
import           Control.Exception              (SomeException)
import           Control.Lens
import           Control.Monad.Except           (ExceptT, runExceptT)
import           Control.Monad.Freer            (Eff)
import           Control.Monad.Error.Class      (throwError, catchError, MonadError)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Identity   (runIdentityT, IdentityT)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (StateT (..))
import           Data.ByteString                (ByteString)
import           Data.Proxy
import           Data.Time.Clock                (UTCTime)
import           Database.Persist.Sql           (SqlBackend)
import           Network.Mail.Mime              (Mail)
import           Servant.Subscriber             (Event, HasLink, IsElem,
                                                 IsSubscribable,
                                                 IsValidEndpoint, MkLink, URI)
import           System.Random                  (StdGen)

import           Gonimo.Database.Effects
import           Gonimo.Server.Db.Entities      (FamilyId)
import           Gonimo.Server.Effects.Logging  as Logging
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
import           Control.Monad.Trans.Reader      (ReaderT)

secretLength :: Int
secretLength = 16

class (Monad m, MonadError SomeException m) => Server m where
  atomically :: STM a -> m a
  registerDelay :: Int -> m Bool
  sendEmail :: Mail -> m ()
  genRandomBytes :: Int -> m ByteString
  getCurrentTime :: m UTCTime
  runDb :: ReaderT SqlBackend IO w -> m a
  runRandom :: (StdGen -> (a, StdGen)) -> m a
  getState :: m OnlineState
  notify :: forall endpoint . (IsElem endpoint GonimoAPI, HasLink endpoint
                              , IsValidEndpoint endpoint, IsSubscribable endpoint GonimoAPI)
            => Event -> Proxy endpoint -> (MkLink endpoint -> URI) -> m ()


generateSecret :: Server m => m Secret
generateSecret = Secret <$> genRandomBytes secretLength


-- | Update family, retrying if updateF returns Nothing
updateFamilyRetryEff :: Server m
                   => ServerError -> FamilyId -> StateT FamilyOnlineState (MaybeT STM) a -> m a
updateFamilyRetryEff err familyId updateF = do
  state <- getState
  timeUp <- registerDelay standardTimeout
  r <- atomically . runMaybeT $ updateFamilyRetry timeUp state familyId updateF
  case r of
    Nothing -> throwServer err
    Just v -> pure v

-- | Update family, throwing any ServerErrors.
updateFamilyErrEff :: (Server m, ToServerError e, Monoid e)
                   => FamilyId -> UpdateFamilyT (ExceptT e STM) a -> MaybeT m a
updateFamilyErrEff familyId updateF = do
  state <- lift getState
  er <- lift . atomically . runExceptT $ updateFamily state familyId updateF
  mayThrowLeft er

-- | May update family if updateF does not return Nothing.
mayUpdateFamilyEff :: Server m
                   => FamilyId -> StateT FamilyOnlineState (MaybeT STM) a -> MaybeT m a
mayUpdateFamilyEff familyId updateF = do
  state <- lift getState
  MaybeT . atomically . runMaybeT $ updateFamily state familyId updateF


-- | Update a family online state.
updateFamilyEff :: Server m
                   => FamilyId -> StateT FamilyOnlineState (IdentityT STM) a -> m a
updateFamilyEff familyId updateF = do
  state <- getState
  atomically . runIdentityT $ updateFamily state familyId updateF


getFamilyEff :: Server m
                =>  FamilyId -> m FamilyOnlineState
getFamilyEff familyId = do
  state <- getState
  mFamily <- atomically $ lookupFamily state familyId
  fromMaybeErr (FamilyNotOnline familyId) mFamily


waitForReaderEff :: forall key val num m. (Ord key, Server m)
                 => ServerError -> FamilyId
                 -> key -> Lens' FamilyOnlineState (MessageBoxes key val num)
                 -> m ()
waitForReaderEff onTimeout familyId key messageBoxes = catchError wait handleNotRead
  where
    wait = updateFamilyRetryEff onTimeout familyId
           $ MsgBox.clearIfRead messageBoxes key

    handleNotRead :: SomeException -> m ()
    handleNotRead e = do
      updateFamilyEff familyId $ MsgBox.clearData messageBoxes key
      throwError e
