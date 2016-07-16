{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM   (STM, TVar, check, modifyTVar,
                                           readTVar, retry)
import           Control.Lens
import           Control.Monad            (forever)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)

import           Gonimo.Server.DbEntities (ClientId, FamilyId)
import           Gonimo.Server.Types      (Secret)

type ChannelSecrets = Map ClientId (ClientId, Secret)
type ChannelData    = Map Secret (ClientId, Text)

data FamilyOnlineState = FamilyOnlineState
                       { _channelSecrets :: ChannelSecrets
                       , _channelData    :: ChannelData
                       , _onlineMembers  :: Set ClientId }
$(makeLenses ''FamilyOnlineState)

type FamilyMap = Map FamilyId (TVar FamilyOnlineState)

type OnlineState = TVar FamilyMap

putSecret :: Secret -> ClientId -> ClientId -> TVar FamilyOnlineState -> STM ()
-- | putSecret inserts possibly overwrites
putSecret secret fromId toId familyStateVar = forever $ do
  t <- _channelSecrets <$> readTVar familyStateVar
  check (toId `M.member` t)
  modifyTVar familyStateVar (channelSecrets %~ toId `M.insert` (fromId, secret))

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
                                     return $ cid `S.member` (familyState^.onlineMembers)

putData :: Text -> Secret -> ClientId -> TVar FamilyOnlineState -> STM ()
-- | putSecret inserts possibly overwrites
putData txt secret fromId familyStateVar = forever $ do
  t <- _channelData <$> readTVar familyStateVar
  if (secret `M.member` t)
     then retry
     else modifyTVar familyStateVar (channelData %~ secret `M.insert` (fromId, txt))

receieveData :: Secret -> TVar FamilyOnlineState -> STM (Maybe (ClientId, Text))
receieveData secret familyStateVar = do
  t <- _channelData <$> readTVar familyStateVar
  case secret `M.lookup` t of
    Nothing ->  return Nothing
    Just cs -> do modifyTVar familyStateVar (channelData %~ M.delete secret)
                  return $ Just cs
