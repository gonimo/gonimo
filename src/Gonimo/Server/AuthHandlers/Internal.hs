{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Server.AuthHandlers.Internal where

import           Control.Concurrent.STM   (STM, TVar, readTVar)
import           Control.Lens             ((^.))
import           Control.Monad.Freer      (Eff)
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S
import           Gonimo.Server.Auth       (AuthServerConstraint,
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

  let fromto = S.fromList [fromId, toId]
  state <- getState
  x <- atomically $ do
    a <- (familyId `M.lookup`) <$> readTVar state
    case a of Nothing -> return Nothing
              Just b -> do c <- readTVar b
                           if fromto `S.isSubsetOf` (c^.onlineMembers)
                              then return Nothing
                              else Just <$> f fromId toId b

  authorizeJust id x


authorizedRecieve :: AuthServerConstraint r
                  => (ClientId -> TVar FamilyOnlineState -> STM (Maybe a))
                  ->  FamilyId -> ClientId -> Eff r a
authorizedRecieve f familyId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  state <- getState
  x <- atomically $ do b <- readTVar state
                       case familyId `M.lookup` b of
                         Nothing -> return Nothing
                         Just d  -> f toId d
  authorizeJust id x



