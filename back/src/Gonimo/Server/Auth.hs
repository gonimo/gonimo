{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.Auth where

import           Control.Lens
import           Control.Monad                 (unless)
import           Control.Monad.Base            (MonadBase)
import           Control.Monad.Reader          (MonadReader, ask)
import           Gonimo.SocketAPI.Types        hiding (AuthData, deviceId, accountId)
import           Gonimo.Server.Error

data AuthData = AuthData { _authAccountId   :: !AccountId
                         , _authAccount     :: !Account
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , _allowedFamilies :: ![FamilyId]
                         , _authDeviceId    :: !DeviceId
                         , _authDevice      :: !Device
                         }
$(makeLenses ''AuthData)

type AuthReader = MonadReader AuthData

type AuthConstraint m = (MonadReader AuthData m, MonadBase IO m)


askAccountId :: AuthReader m => m AccountId
askAccountId = _authAccountId <$> ask

authView :: AuthReader m => Getter AuthData a -> m a
authView g = (^. g) <$> ask

deviceKey :: AuthData -> DeviceId
deviceKey = _authDeviceId

accountKey :: AuthData -> AccountId
accountKey = _authAccountId

isFamilyMember :: FamilyId -> AuthData -> Bool
isFamilyMember fid = (fid `elem`) . _allowedFamilies

isDevice :: DeviceId -> AuthData -> Bool
isDevice deviceId = (== deviceId) . _authDeviceId

isAccount :: AccountId -> AuthData -> Bool
isAccount accountId = (== accountId) . _authAccountId

authorize :: MonadBase IO m => (a -> Bool) -> a -> m ()
authorize check x = unless (check x) (throwServer Forbidden)

authorizeJust :: MonadBase IO m => (a -> Maybe b) -> a -> m b
authorizeJust check = fromMaybeErr Forbidden . check

authorizeAuthData :: (AuthReader m, MonadBase IO m) => (AuthData -> Bool) -> m ()
authorizeAuthData check = authorize check =<< ask
