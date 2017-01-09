{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Gonimo.Server.Error where


import           Control.Exception             (Exception)
import           Control.Monad                 (unless, MonadPlus, (<=<))
import           Control.Monad.Base             (MonadBase)
import           Control.Exception.Lifted       (throwIO)
import           Control.Monad.Trans.Maybe     (MaybeT)
import           Data.Aeson
import           GHC.Generics
import           Gonimo.Db.Entities      (FamilyId, DeviceId)
import           Control.Monad.Trans.Class            (lift)
import           Data.Typeable (Typeable)

-- Define an error type, so handling errors is easier at the client side.
data ServerError = InvalidAuthToken
                 | NotAuthenticated
                 | InvitationAlreadyClaimed -- ^ Invitation was already claimed by someone else.
                 | AlreadyFamilyMember -- ^ If a client tries to become a member of a family he is already a member of.
                 | NoSuchDevice DeviceId -- ^ The device could not be found in the database.
                 | NoSuchFamily FamilyId
                 | FamilyNotOnline FamilyId
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

fromMaybeErr :: MonadBase IO m => ServerError -> Maybe a -> m a
fromMaybeErr err ma = case ma of
  Nothing -> throwServer err
  Just a  -> return a

throwLeft :: MonadBase IO m => Either ServerError a -> m a
throwLeft = either throwServer return

-- | Throw left if actually a ServerError, otherwise return Nothing
mayThrowLeft :: (MonadBase IO m, ToServerError e) => Either e a -> MaybeT m a
mayThrowLeft = either (lift . throwServer <=< toServerError) return

throwServer :: MonadBase IO m => ServerError -> m a
throwServer = throwIO

guardWith :: MonadBase IO m => ServerError -> Bool -> m ()
guardWith exc cond = unless cond $ throwServer exc

guardWithM :: MonadBase IO m => ServerError -> m Bool -> m ()
guardWithM exc mCond = guardWith exc =<< mCond

throwException :: (MonadBase IO m, Exception e) => e -> m a
throwException = throwIO

instance ToJSON ServerError where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerError
