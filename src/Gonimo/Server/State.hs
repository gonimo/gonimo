{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.State where

import           Control.Concurrent.STM    (STM, TVar, modifyTVar, newTVar,
                                            readTVar)
import           Control.Lens
import           Control.Monad             (MonadPlus (mzero), when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           Data.Text                 (Text)

import           Gonimo.Server.DbEntities  (DeviceId, FamilyId)
import           Gonimo.Server.Error       (ServerError (TransactionTimeout),
                                            makeServantErr)
import           Gonimo.Server.Types       (DeviceType, Secret)

import           Utils.Constants           (Microseconds)
import           Utils.STM

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

type PleaseNameMeT m a = StateT FamilyOnlineState (MaybeT m) a

emptyFamily :: FamilyOnlineState
emptyFamily = FamilyOnlineState {
    _channelSecrets = M.empty
  , _channelData = M.empty
  , _onlineMembers = M.empty
  }

putSecret :: Monad m => Secret -> FromId -> ToId -> PleaseNameMeT m ()
putSecret secret fromId toId = do
  secrets <- gets _channelSecrets
  case (fromId ==) . fst <$> toId `M.lookup` secrets of
    Just True -> mzero
    _         -> modify (channelSecrets %~ toId `M.insert` (fromId, secret))

receiveSecret :: Monad m => DeviceId -> PleaseNameMeT m (DeviceId, Secret)
receiveSecret toId = do
  secrets <- gets _channelSecrets
  case toId `M.lookup` secrets of
    Nothing -> mzero
    Just cs -> do modify (channelSecrets %~ M.delete toId)
                  return cs

onlineMember :: Monad m => DeviceId -> PleaseNameMeT m Bool
-- | TODO: does not actually change the state - should this be explicit in the
-- type signature ??
onlineMember cid = do
  memberKeys <- gets (M.keysSet . _onlineMembers)
  return $ cid `S.member` memberKeys

putData :: Monad m => Text -> (FromId, ToId, Secret) -> PleaseNameMeT m ()
putData txt fromToSecret = do
  cdata <- gets _channelData
  if fromToSecret `M.member` cdata
     then mzero
     else modify (channelData %~ fromToSecret `M.insert` txt)

receieveData :: Monad m => (FromId, ToId, Secret) -> PleaseNameMeT m Text
receieveData fromToSecret = do
  cdata <- gets _channelData
  case fromToSecret `M.lookup` cdata of
    Nothing  -> mzero
    Just txt -> do modify (channelData %~ M.delete fromToSecret)
                   return txt

-- | Update a family.
--
--   If `onlineMembers` is empty after the update the Family will be removed from the map.
--   If the family does not exist yet - it will be created.
--
-- TODO: CHANGE Type Signature to ... (FamilyOnlineState -> MAYBE FamilyOnlineState) -> STM ()
-- using the timeout function from Gonimo.Util.STM
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

-- TODO: Should this be ... -> PleaseNameMeT m () - to be coherent and timeoutable 
updateStatus :: (DeviceId, DeviceType) -> FamilyOnlineState -> FamilyOnlineState
updateStatus (clientId, clientType) = onlineMembers . at clientId .~ Just clientType

-- TODO: Should this be ... -> PleaseNameMeT m () - to be coherent and timeoutable 
deleteStatus :: DeviceId -> FamilyOnlineState -> FamilyOnlineState
deleteStatus clientId = onlineMembers . at clientId .~ Nothing
