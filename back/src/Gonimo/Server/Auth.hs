{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.Auth where

import           Control.Lens
import           Control.Monad          (unless)
import           Control.Monad.Base     (MonadBase)
import           Control.Monad.Reader   (MonadReader)
import           Control.Monad.RIO
import           Control.Monad.Catch  as X (MonadThrow (..))

import           Gonimo.Server.Error
import           Gonimo.SocketAPI.Types hiding (AuthData, accountId, deviceId)

data AuthData = AuthData { _authAccountId   :: !AccountId
                         , _authAccount     :: !Account
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , _allowedFamilies :: ![FamilyId]
                         , _authDeviceId    :: !DeviceId
                         , _authDevice      :: !Device
                         }
$(makeClassy ''AuthData)

type AuthReader = MonadReader AuthData

type AuthConstraint m = (MonadReader AuthData m, MonadBase IO m)


{-# DEPRECATED askAccountId "Use 'view authAccountId' instead." #-}
askAccountId :: HasAuthData env => RIO env AccountId
askAccountId = view authAccountId

{-# DEPRECATED authView "Use view instead." #-}
authView :: HasAuthData env => Getter env a -> RIO env a
authView = view

{-# DEPRECATED deviceKey "Use authDeviceId instead." #-}
deviceKey :: AuthData -> DeviceId
deviceKey = _authDeviceId

{-# DEPRECATED accountKey "Use authAccountId instead." #-}
accountKey :: AuthData -> AccountId
accountKey = _authAccountId

-- * Property checkers for Authentication:
--
--   Those checkers operate on a 'MonadReader' instance. Note:
--   Due to the fact that (env ->) has a 'MonadReader' instance, those
--   functions can be used as pure functions. 'isFamilyMember' for example can
--   be seen as if it had the signature:
--
-- > isFamilyMember :: HasAuthData env => FamilyId -> env -> Bool
--
--   and used with authorize accordingly:
--
--   authorize $ isFamilyMember someFamilyId myAuthData

-- | Check whether the given 'FamilyId' is member of on of our families.
isFamilyMember :: (HasAuthData env, MonadReader env m) => FamilyId -> m Bool
isFamilyMember fid = (fid `elem`) <$> view allowedFamilies

-- | Check whether the given 'DeviceId' is us (the currently connected client).
isDevice :: (HasAuthData env, MonadReader env m) => DeviceId -> m Bool
isDevice deviceId = (== deviceId) <$> view authDeviceId

-- | Check whether the given 'AccountId' is us (belongs to the currently connected client).
isAccount :: (HasAuthData env, MonadReader env m) => AccountId -> m Bool
isAccount accountId = (== accountId) <$> view authAccountId


-- * Authorize access:

-- | Throws 'Forbidden' if called with False, otherwise does nothing.
--
--   Expected to be called something like this:
--
-- > authorize =<< isFamilyMember fid
--
--   Or in a pure setting (isFamilyMember gets simply applied as a function to a given authData):
--
-- > authorize $ isFamilyMember (invitationFamilyId inv) authData
--
authorize :: MonadThrow m => Bool -> m ()
authorize check = unless check (throwM Forbidden)

-- | Similar to 'authorize', but throws on Nothing.
--
--   A 'Just' value is just passed on. On 'Nothing', this function throws
--   'Forbidden'.
authorizeJust :: MonadThrow m => Maybe a -> m a
authorizeJust check = fromMaybeErr Forbidden check
