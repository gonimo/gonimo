module Gonimo.Server.AuthHandlers.Internal where

import           Control.Concurrent.STM   (STM, TVar, readTVar)
import           Control.Lens             ((^.))
import           Control.Monad.Freer      (Eff)
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S
import           Gonimo.Server.Auth       (AuthServerConstraint, authorize,
                                           authorizeAuthData, authorizeJust,
                                           clientKey, isFamilyMember)
import           Gonimo.Server.DbEntities (ClientId, FamilyId)
import           Gonimo.Server.Effects    (atomically, getState)
import           Gonimo.Server.State      (FamilyOnlineState, onlineMembers)



authorizedPut :: AuthServerConstraint r
              => (ClientId -> ClientId -> TVar FamilyOnlineState -> STM ())
              ->  FamilyId -> ClientId -> ClientId -> Eff r ()
authorizedPut f familyId fromId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . clientKey)

  familyOnlineData  <- authorizeJust (familyId `M.lookup`)
                    =<< atomically . readTVar
                    =<< getState

  familyOnlineState <- atomically $ readTVar familyOnlineData
  let fromto = S.fromList [fromId, toId]

  authorize (fromto `S.isSubsetOf`) (familyOnlineState^.onlineMembers)

  atomically $ f fromId toId familyOnlineData


authorizedRecieve :: AuthServerConstraint r
                  => (ClientId -> TVar FamilyOnlineState -> STM (Maybe a))
                  ->  FamilyId -> ClientId -> Eff r a
authorizedRecieve f familyId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  familyOnlineData  <- authorizeJust (familyId `M.lookup`)
                    =<< atomically . readTVar
                    =<< getState

  authorizeJust id =<< atomically (f toId familyOnlineData)
