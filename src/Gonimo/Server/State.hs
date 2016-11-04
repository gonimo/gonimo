{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM    (STM, TVar, modifyTVar', newTVar,
                                            readTVar, retry, writeTVar)
import           Control.Lens
import           Control.Monad             (MonadPlus (mzero), unless, when)
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
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

putData :: Monad m => Text -> (FromId, ToId, Secret) -> UpdateFamilyT m ()
putData txt fromToSecret = do
  cdata <- gets _channelData
  if fromToSecret `M.member` cdata
     then mzero
     else channelData.at fromToSecret .= Just (Written txt)

receiveData :: Monad m => (FromId, ToId, Secret) -> UpdateFamilyT m Text
receiveData fromToSecret = do
  cdata <- gets _channelData
  txt <- maybe mzero return $ cdata^?at fromToSecret . _Just . _Written
  channelData.at fromToSecret .= Just Read
  return $ txt

-- Status/session API:
-----

-- | Update a family.
--
--   If `onlineMembers` is empty after the update the Family will be removed from the map.
--   If the family does not exist yet - it will be created.
--
updateFamily :: OnlineState -> FamilyId -> UpdateFamily a -> STM (Maybe a)
updateFamily families familyId f = do
    oldFamily <- getFamily

    let (result, newFamily) =
          case runIt f oldFamily of
            Nothing -> (Nothing, oldFamily)
            Just (val, outFamily) -> (Just val, outFamily)

    writeFamily newFamily
    return result
  where

    runIt :: UpdateFamily a -> FamilyOnlineState -> Maybe (a, FamilyOnlineState)
    runIt f' = runIdentity . runMaybeT . runStateT f'

    getFamily :: STM FamilyOnlineState
    getFamily = do
      familiesP <- readTVar families
      case familiesP ^. at familyId of
        Nothing -> return emptyFamily
        Just oldFamily -> readTVar oldFamily

    writeFamily :: FamilyOnlineState -> STM ()
    writeFamily newFamily = do
      familiesP <- readTVar families
      case familiesP ^. at familyId of
        Nothing -> unless (newFamily ^. sessions . to M.null) $ do
          newFamilyTVar <- newTVar newFamily
          modifyTVar' families $ at familyId .~ Just newFamilyTVar
        Just familyTVar ->
          if newFamily ^. sessions . to M.null -- Cleanup needed?
          then modifyTVar' families $ at familyId .~ Nothing
          else writeTVar familyTVar newFamily -- Ok just write value.

-- | Like updateFamily but retries until timeUp becomes true.
updateFamilyRetry :: TVar Bool -> OnlineState -> FamilyId -> UpdateFamily a -> STM (Maybe a)
updateFamilyRetry timeUp families familyId f = do
  timeUp' <- readTVar timeUp
  if timeUp'
    then pure Nothing
    else do
    r <- updateFamily families familyId f
    case r of
      Nothing -> retry
      Just _ -> pure r


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
