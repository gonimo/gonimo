{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Gonimo.Server.Error where


import           Control.Exception             (Exception)
import           Control.Monad                 (unless, MonadPlus, (<=<))
import           Control.Monad.Base             (MonadBase)
import           Control.Exception.Lifted       (throwIO)
import           Control.Monad.Trans.Maybe     (MaybeT)
import           Data.Aeson
import           GHC.Generics
import           Gonimo.Db.Entities      (FamilyId, DeviceId)
import           Servant.Server
import           Control.Monad.Trans.Class            (lift)
import           Gonimo.Server.State.Types      (MessageNumber, ChannelRequest)

-- Define an error type, so handling errors is easier at the client side.
data ServerError = InvalidAuthToken
                 | InvitationAlreadyClaimed -- ^ Invitation was already claimed by someone else.
                 | AlreadyFamilyMember -- ^ If a client tries to become a member of a family he is already a member of.
                 | NoSuchDevice DeviceId -- ^ The device could not be found in the database.
                 | NoSuchFamily FamilyId
                 | FamilyNotOnline FamilyId
                 | NoSuchInvitation
                 | SocketBusy
                 | ChannelBusy
                 | NoSuchSocket
                 | NoSuchChannel
                 | Forbidden
                 | NotFound
                 | MessageAlreadyGone MessageNumber
                 | ChannelAlreadyGone ChannelRequest
                 | TransactionTimeout
                 | SessionInvalid -- There exists a session for this device, but it does not match
                 | NoActiveSession -- There is no sessino for this device.
                 | InternalServerError
  deriving (Generic)

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
throwServer = throwServant . makeServantErr

guardWith :: MonadBase IO m => ServerError -> Bool -> m ()
guardWith exc cond = unless cond $ throwServer exc

guardWithM :: MonadBase IO m => ServerError -> m Bool -> m ()
guardWithM exc mCond = guardWith exc =<< mCond

throwServant :: MonadBase IO m => ServantErr -> m a
throwServant = throwIO

throwException :: (MonadBase IO m, Exception e) => e -> m a
throwException = throwIO

makeServantErr :: ServerError -> ServantErr
makeServantErr err = (getServantErr err) { errBody = encode err }


-- | Internal
getServantErr :: ServerError -> ServantErr
getServantErr InvalidAuthToken         = err403
getServantErr InvitationAlreadyClaimed = err403
getServantErr AlreadyFamilyMember      = err409
getServantErr (NoSuchFamily _)         = err404
getServantErr (FamilyNotOnline _)      = err404
getServantErr NoSuchInvitation         = err404
getServantErr SocketBusy               = err503
getServantErr ChannelBusy              = err503
getServantErr NoSuchSocket             = err404
getServantErr (MessageAlreadyGone _)   = err410
getServantErr (ChannelAlreadyGone _)   = err410
getServantErr NoSuchChannel            = err404
getServantErr (NoSuchDevice _)         = err404
getServantErr NotFound                 = err404
getServantErr Forbidden                = err403
getServantErr TransactionTimeout       = err500
getServantErr SessionInvalid           = err409
getServantErr NoActiveSession          = err404
getServantErr InternalServerError      = err500

instance ToJSON ServerError where
    toJSON = genericToJSON defaultOptions
