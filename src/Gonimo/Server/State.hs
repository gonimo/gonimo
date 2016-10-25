{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM    (STM, TVar, modifyTVar, newTVar,
                                            readTVar, writeTVar, retry)
import           Control.Lens
import           Control.Monad             (MonadPlus (mzero), unless)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           Data.Text                 (Text)

import           Gonimo.Server.DbEntities  (DeviceId, FamilyId)
import           Gonimo.Server.Types       (DeviceType, Secret)

type FromId = DeviceId
type ToId   = DeviceId

-- | Baby station calls receiveSocket: Map of it's client id to the requester's client id and the channel secret.
type ChannelSecrets = Map ToId (FromId, Secret)

type ChannelData a  = Map (FromId, ToId, Secret) a

data FamilyOnlineState = FamilyOnlineState
                       { _channelSecrets :: ChannelSecrets
                       , _channelData    :: ChannelData Text
                       , _onlineMembers  :: Map DeviceId DeviceType
                       } deriving (Show, Eq)

$(makeLenses ''FamilyOnlineState)

type FamilyMap = Map FamilyId (TVar FamilyOnlineState)

type OnlineState = TVar FamilyMap

type UpdateFamilyT m a = StateT FamilyOnlineState (MaybeT m) a
type UpdateFamily a = UpdateFamilyT Identity a

emptyFamily :: FamilyOnlineState
emptyFamily = FamilyOnlineState {
    _channelSecrets = M.empty
  , _channelData = M.empty
  , _onlineMembers = M.empty
  }

onlineMember :: Monad m => DeviceId -> UpdateFamilyT m Bool
-- | TODO: does not actually change the state - should this be explicit in the
-- type signature ??
onlineMember cid = do
  memberKeys <- gets (M.keysSet . _onlineMembers)
  return $ cid `S.member` memberKeys

putData :: Monad m => Text -> (FromId, ToId, Secret) -> UpdateFamilyT m ()
putData txt fromToSecret = do
  cdata <- gets _channelData
  if fromToSecret `M.member` cdata
     then mzero
     else channelData.at fromToSecret .= Just txt

receiveData :: Monad m => (FromId, ToId, Secret) -> UpdateFamilyT m Text
receiveData fromToSecret = do
  cdata <- gets _channelData
  txt <- maybe mzero return $ cdata^.at fromToSecret
  channelData.at fromToSecret .= Nothing
  return txt

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
        Nothing -> unless (newFamily ^. onlineMembers . to M.null) $ do
          newFamilyTVar <- newTVar newFamily
          modifyTVar families $ at familyId .~ Just newFamilyTVar
        Just familyTVar ->
          if newFamily ^. onlineMembers . to M.null -- Cleanup needed?
          then modifyTVar families $ at familyId .~ Nothing
          else writeTVar familyTVar newFamily -- Ok just write value.

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

updateStatus :: (DeviceId, DeviceType) -> UpdateFamily ()
updateStatus (clientId, clientType) = onlineMembers.at clientId .= Just clientType

deleteStatus :: DeviceId -> UpdateFamily ()
deleteStatus clientId = onlineMembers.at clientId .= Nothing
