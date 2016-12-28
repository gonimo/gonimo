{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM    (STM, TVar, modifyTVar', newTVar,
                                            readTVar, retry, writeTVar)
import           Control.Lens
import           Control.Applicative       ((<|>))
import           Control.Monad             (MonadPlus, unless, guard)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State (StateT (..))
import qualified Data.Map.Strict           as M

import           Gonimo.Db.Entities (FamilyId)
import           Gonimo.Server.State.Types

-- | Update a family.
--
--   If `onlineMembers` is empty after the update the Family will be removed from the map.
--   If the family does not exist yet - it will be created.
--
updateFamily :: (Monad (t STM), MonadTrans t) =>
                OnlineState -> FamilyId -> StateT FamilyOnlineState (t STM) a -> t STM a
updateFamily families familyId f = do
    oldFamily <- lift $ getFamily families familyId
    (r, newFamily) <- runStateT f oldFamily
    lift $ writeFamily families familyId newFamily
    return $ r

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

