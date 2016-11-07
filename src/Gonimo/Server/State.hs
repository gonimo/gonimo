{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM    (STM, TVar, modifyTVar', newTVar,
                                            readTVar, retry, writeTVar)
import           Control.Lens
import           Control.Applicative       ((<|>))
import           Control.Monad             (MonadPlus (mzero), unless, when, guard)
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (StateT (..))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           Data.Text                 (Text)

import           Gonimo.Server.Db.Entities (DeviceId, FamilyId)
import           Gonimo.Server.Error       (ServerError (NoActiveSession, SessionInvalid),
                                            ToServerError, toServerError)
import           Gonimo.Server.State.Types
import           Gonimo.Server.Types       (DeviceType, Secret)

putData :: (MonadState FamilyOnlineState m, Monad m, MonadPlus m)
           => Text -> (FromId, ToId, Secret) -> m ()
putData txt fromToSecret = do
  cdata <- gets _channelData
  if fromToSecret `M.member` cdata
     then mzero
     else channelData.at fromToSecret .= Just (Written txt)

receiveData :: (Monad m, MonadPlus m, MonadState FamilyOnlineState m)
               => (FromId, ToId, Secret) -> m Text
receiveData fromToSecret = do
  cdata <- gets _channelData
  txt <- maybe mzero return $ cdata^?at fromToSecret . _Just . _Written
  channelData.at fromToSecret .= Just Read
  return $ txt


-- | Update a family.
--
--   If `onlineMembers` is empty after the update the Family will be removed from the map.
--   If the family does not exist yet - it will be created.
--
updateFamily :: (Monad (t STM), MonadTrans t) =>
                OnlineState -> FamilyId -> StateT FamilyOnlineState (t STM) a -> t STM a
updateFamily families familyId f = do
    oldFamily <- lift $ getFamily families familyId
    r <- runStateT f oldFamily
    lift $ writeFamily families familyId (r^._2)
    return $ r^._1

-- | Like updateFamily but retries until timeUp becomes true.
updateFamilyRetry :: (MonadPlus (t STM), MonadTrans t) =>
                     TVar Bool -> OnlineState -> FamilyId -> StateT FamilyOnlineState (t STM) a
                  -> t STM a
updateFamilyRetry timeUp families familyId f = do
  timeUp' <- lift $ readTVar timeUp
  guard (not timeUp')
  updateFamily families familyId f <|> lift retry


lookupFamily :: OnlineState -> FamilyId -> STM (Maybe FamilyOnlineState)
lookupFamily families familyId= do
  familiesP <- readTVar families
  traverse readTVar $ M.lookup familyId familiesP


data CleanReceivedResult = WasReceived
                      | WasNotReceived
                      | AlreadyCleaned
                      | FamilyNotFoundError

-- | Block until a value was received, if timeUp becomes true earlier, we clean the queue and return `WeCleared`.
cleanReceived :: forall a. OnlineState -> FamilyId -> TVar Bool
              -> Lens' FamilyOnlineState (Maybe (QueueStatus a))
              -> STM CleanReceivedResult
cleanReceived families familyId timeUp queue = do
  familiesP <- readTVar families
  case familiesP ^. at familyId of
    Nothing -> pure FamilyNotFoundError
    Just family -> do
      timeUp' <- readTVar timeUp
      if timeUp'
        then do
        modifyTVar' family $ queue .~ Nothing
        pure WasNotReceived
        else do
        queueValue <- (^. queue) <$> readTVar family
        case queueValue of
          Nothing           -> pure AlreadyCleaned
          Just Read         -> do
            modifyTVar' family $ queue .~ Nothing
            pure WasReceived
          Just (Written _)  -> retry

--  Internal helper functions

-- | Get a family - creating one if not yet existing.
getFamily :: OnlineState -> FamilyId -> STM FamilyOnlineState
getFamily families familyId = do
  familiesP <- readTVar families
  case familiesP ^. at familyId of
    Nothing -> return emptyFamily
    Just oldFamily -> readTVar oldFamily


-- | Write a family back
writeFamily :: OnlineState -> FamilyId -> FamilyOnlineState -> STM ()
writeFamily families familyId newFamily = do
  familiesP <- readTVar families
  case familiesP ^. at familyId of
    Nothing -> unless (newFamily ^. sessions . to M.null) $ do
      newFamilyTVar <- newTVar newFamily
      modifyTVar' families $ at familyId .~ Just newFamilyTVar
    Just familyTVar ->
      if newFamily ^. sessions . to M.null -- Cleanup needed?
      then modifyTVar' families $ at familyId .~ Nothing
      else writeTVar familyTVar newFamily -- Ok just write value.

