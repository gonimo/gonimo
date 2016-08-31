{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar,
                                         readTVar, retry)
import           Control.Lens
import           Control.Monad          (when)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Text              (Text)

import           Gonimo.Server.DbEntities (ClientId, FamilyId)
import           Gonimo.Server.Types      (Secret, ClientType)

-- | Baby station calls receiveSocket: Map of it's client id to the requester's client id and the channel secret.
type ChannelSecrets = Map ClientId (ClientId, Secret)
type ChannelData    = Map Secret (ClientId, Text)

data FamilyOnlineState = FamilyOnlineState
                       { _channelSecrets :: ChannelSecrets
                       , _channelData    :: ChannelData
                       , _onlineMembers  :: Map ClientId ClientType
                       } deriving (Show, Eq)

$(makeLenses ''FamilyOnlineState)

type FamilyMap = Map FamilyId (TVar FamilyOnlineState)

type OnlineState = TVar FamilyMap

emptyFamily :: FamilyOnlineState
emptyFamily = FamilyOnlineState {
    _channelSecrets = M.empty
  , _channelData = M.empty
  , _onlineMembers = M.empty
  }

putSecret :: Secret -> ClientId -> ClientId -> TVar FamilyOnlineState -> STM ()
-- | putSecret inserts possibly overwrites
putSecret secret fromId toId familyStateVar = do
  t <- _channelSecrets <$> readTVar familyStateVar
  if toId `M.member` t
     then retry
     else modifyTVar familyStateVar (channelSecrets %~ toId `M.insert` (fromId, secret))

receiveSecret :: ClientId -> TVar FamilyOnlineState -> STM (Maybe (ClientId, Secret))
receiveSecret toId familyStateVar = do
  t <- _channelSecrets <$> readTVar familyStateVar
  case toId `M.lookup` t of
    Nothing ->  return Nothing
    Just cs -> do modifyTVar familyStateVar (channelSecrets %~ M.delete toId)
                  return $ Just cs

--deleteSecret :: ClientId -> ChannelSecrets -> ChannelSecrets
--deleteSecret toId = undefined -- ChannelSecrets . M.delete toId . fetch

onlineMember :: ClientId -> TVar FamilyOnlineState -> STM Bool
onlineMember cid familyStateVar = do familyState <- readTVar familyStateVar
                                     return $ cid `S.member` (M.keysSet $ familyState^.onlineMembers)

putData :: Text -> Secret -> ClientId -> TVar FamilyOnlineState -> STM ()
-- | putSecret inserts possibly overwrites
putData txt secret fromId familyStateVar = do
  t <- _channelData <$> readTVar familyStateVar
  if secret `M.member` t
     then retry
     else modifyTVar familyStateVar (channelData %~ secret `M.insert` (fromId, txt))

receieveData :: Secret -> TVar FamilyOnlineState -> STM (Maybe (ClientId, Text))
receieveData secret familyStateVar = do
  t <- _channelData <$> readTVar familyStateVar
  case secret `M.lookup` t of
    Nothing ->  return Nothing
    Just cs -> do modifyTVar familyStateVar (channelData %~ M.delete secret)
                  return $ Just cs

-- | Update a family.
--
--   If `onlineMembers` is empty after the update the Family will be removed from the map.
--   If the family does not exist yet - it will be created.
updateFamily :: OnlineState -> FamilyId -> (FamilyOnlineState -> FamilyOnlineState) -> STM ()
updateFamily families familyId f = do
  familiesP <- readTVar families
  let mFamily = M.lookup familyId familiesP
  family <- case mFamily of
      Nothing        -> do
        newFamily <- newTVar emptyFamily
        modifyTVar families $ at familyId .~ Just newFamily
        return newFamily
      Just oldFamily -> return oldFamily
  modifyTVar family f
  isEmpty <- view (onlineMembers . to M.null) <$> readTVar family
  when isEmpty
    $ modifyTVar families (at familyId .~ Nothing)

lookupFamily :: OnlineState -> FamilyId -> STM (Maybe FamilyOnlineState)
lookupFamily families familyId= do
  familiesP <- readTVar families
  traverse readTVar $ M.lookup familyId familiesP

updateStatus :: (ClientId, ClientType) -> FamilyOnlineState -> FamilyOnlineState
updateStatus (clientId, clientType) = onlineMembers . at clientId .~ Just clientType

deleteStatus :: ClientId -> FamilyOnlineState -> FamilyOnlineState
deleteStatus clientId = onlineMembers . at clientId .~ Nothing
