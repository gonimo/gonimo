{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.Auth where

import           Control.Lens
import           Control.Monad                 (unless)
import           Control.Monad.Base            (MonadBase)
import           Control.Monad.Reader          (MonadReader, ask)
import           Database.Persist              (Entity (..), Key)
import           Gonimo.Db.Entities
import           Gonimo.Server.Error

data AuthData = AuthData { _accountEntity   :: Entity Account
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , _allowedFamilies :: [FamilyId]
                         , _deviceEntity    :: Entity Device
                         }
$(makeLenses ''AuthData)

type AuthReader = MonadReader AuthData

type AuthConstraint m = (MonadReader AuthData m, MonadBase IO m)


askAccountId :: AuthReader m => m (Key Account)
askAccountId = entityKey . _accountEntity <$> ask

authView :: AuthReader m => Getter AuthData a -> m a
authView g = (^. g) <$> ask

deviceKey :: AuthData -> Key Device
deviceKey = entityKey . _deviceEntity

accountKey :: AuthData -> Key Account
accountKey = entityKey . _accountEntity

isFamilyMember :: FamilyId -> AuthData -> Bool
isFamilyMember fid = (fid `elem`) . _allowedFamilies

isDevice :: DeviceId -> AuthData -> Bool
isDevice deviceId = (== deviceId) . entityKey . _deviceEntity

isAccount :: AccountId -> AuthData -> Bool
isAccount accountId = (== accountId) . entityKey . _accountEntity

authorize :: MonadBase IO m => (a -> Bool) -> a -> m ()
authorize check x = unless (check x) (throwServer Forbidden)

authorizeJust :: MonadBase IO m => (a -> Maybe b) -> a -> m b
authorizeJust check x = fromMaybeErr Forbidden . check $ x

authorizeAuthData :: (AuthReader m, MonadBase IO m) => (AuthData -> Bool) -> m ()
authorizeAuthData check = authorize check =<< ask
