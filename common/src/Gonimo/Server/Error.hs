{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
module Gonimo.Server.Error where


import           Control.Exception         (Exception)

import           Control.Monad             (MonadPlus, unless, (<=<))
import           Control.Monad.Catch       as X (MonadThrow (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Aeson
import           Data.Typeable             (Typeable)
import           GHC.Generics

import           Gonimo.SocketAPI.Types    (AccountId, DeviceId, FamilyId)

-- Define an error type, so handling errors is easier at the client side.
data ServerError = InvalidAuthToken
                 | NotAuthenticated
                 | InvitationAlreadyClaimed -- ^ Invitation was already claimed by someone else.
                 | AlreadyFamilyMember !FamilyId-- ^ If a client tries to become a member of a family he is already a member of.
                 | NoSuchDevice !DeviceId -- ^ The device could not be found in the database.
                 | NoSuchAccount !AccountId
                 | NoSuchFamily !FamilyId
                 | FamilyNotOnline !FamilyId
                 | CantSendInvitation
                 | NoSuchInvitation
                 | DeviceOffline -- ^ You tried to send a message to an offline device.
                 | Forbidden
                 | NotFound
                 | TransactionTimeout
                 | SessionInvalid -- There exists a session for this device, but it does not match
                 | NoActiveSession -- There is no sessino for this device.
                 | InternalServerError deriving (Generic, Eq, Show, Typeable)

instance Exception ServerError

-- | Errors that can be converted to a ServerError.
class ToServerError e where
  toServerError :: MonadPlus m => e -> m ServerError

instance ToServerError ServerError where
  toServerError = return

fromMaybeErr :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
fromMaybeErr err ma = case ma of
  Nothing -> throwM err
  Just a  -> return a

throwLeft :: MonadThrow m => Either ServerError a -> m a
throwLeft = either throwServer return

-- | Throw left if actually a ServerError, otherwise return Nothing
mayThrowLeft :: (MonadThrow m, ToServerError e) => Either e a -> MaybeT m a
mayThrowLeft = either (lift . throwServer <=< toServerError) return

{-# Deprecated throwServer "Use throwM  directly instead." #-}
throwServer :: MonadThrow m => ServerError -> m a
throwServer = throwM

guardWith :: MonadThrow m => ServerError -> Bool -> m ()
guardWith exc cond = unless cond $ throwServer exc

guardWithM :: MonadThrow m => ServerError -> m Bool -> m ()
guardWithM exc mCond = guardWith exc =<< mCond

{-# Deprecated throwException "Use throwM directly instead." #-}
throwException :: (MonadThrow m, Exception e) => e -> m a
throwException = throwM

instance ToJSON ServerError where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerError
