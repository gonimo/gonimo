{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Server.AuthHandlers.Internal where

import           Control.Concurrent.STM          (STM, TVar, readTVar)
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.STM.Class         (liftSTM)
import           Control.Monad.Trans.Maybe       (MaybeT (..), runMaybeT)
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import           Gonimo.Server.Auth              (AuthServerConstraint,
                                                  authorizeAuthData,
                                                  authorizeJust, clientKey,
                                                  isFamilyMember)
import           Gonimo.Server.DbEntities        (ClientId, FamilyId)
import           Gonimo.Server.Effects           (atomically, getState)
import           Gonimo.Server.Error
import           Gonimo.Server.State             (FamilyOnlineState,
                                                  onlineMembers)
import           Utils.Control.Monad.Trans.Maybe (maybeT)
import           System.Timeout


authorizedPut :: AuthServerConstraint r
              => (TVar FamilyOnlineState -> STM ())
              ->  FamilyId -> ClientId -> ClientId -> Eff r ()
authorizedPut f familyId fromId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  let fromto = S.fromList [fromId, toId]
  state <- getState
  x <- atomically $ runMaybeT $ do
    a <- (maybeT . (familyId `M.lookup`)) =<< liftSTM (readTVar state)
    b <- liftSTM $ readTVar a
    guard $ fromto `S.isSubsetOf` (b^.onlineMembers)
    liftSTM $ f a

  authorizeJust id x


authorizedRecieve :: AuthServerConstraint r
                  => (TVar FamilyOnlineState -> STM (Maybe a))
                  ->  FamilyId -> ClientId -> ClientId -> Eff r a
authorizedRecieve f familyId fromId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  let fromto = S.fromList [fromId, toId]
  state <- getState
  x <- atomically $ runMaybeT $ do
    a <- (maybeT . (familyId `M.lookup`)) =<< liftSTM (readTVar state)
    b <- liftSTM $ readTVar a
    guard $ fromto `S.isSubsetOf` (b^.onlineMembers)
    MaybeT $ f a
  authorizeJust id x

authorizedRecieve' :: AuthServerConstraint r
                  => (TVar FamilyOnlineState -> STM (Maybe a))
                  ->  FamilyId -> ClientId -> Eff r a
authorizedRecieve' f familyId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  state <- getState
  x <- atomically $ runMaybeT $ do
    a <- (maybeT . (familyId `M.lookup`)) =<< liftSTM (readTVar state)
    MaybeT $ f a
  authorizeJust id x
