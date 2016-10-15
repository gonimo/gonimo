{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Gonimo.Server.Error where


import           Control.Exception             (Exception, SomeException,
                                                toException)
import           Control.Monad                 (unless)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Data.Aeson
import           Data.Typeable                 (Typeable)
import           GHC.Generics
import           Gonimo.Server.DbEntities      (FamilyId)
import           Servant.Server

-- Define an error type, so handling errors is easier at the client side.
-- This makes it easier to handle them at the client side.
data ServerError = InvalidAuthToken
                 | InvitationAlreadyClaimed -- ^ Invitation was already claimed by someone else.
                 | AlreadyFamilyMember -- ^ If a client tries to become a member of a family he is already a member of.
                 | NoSuchFamily FamilyId
                 | NoSuchInvitation
                 | NoSuchSocket
                 | Forbidden
                 | NotFound
                 | NoDataAvailable
                 | NoNewChannel
                 | TransactionTimeout
                 | NotPossible -- For errors that actually cannot really happen.
  deriving (Generic)

fromMaybeErr :: Member (Exc SomeException) r => ServerError -> Maybe a -> Eff r a
fromMaybeErr err ma = case ma of
  Nothing -> throwServer err
  Just a  -> return a

throwServer :: Member (Exc SomeException) r => ServerError -> Eff r a
throwServer = throwServant . makeServantErr

guardWith :: Member (Exc SomeException) r => ServerError -> Bool -> Eff r ()
guardWith exc cond = unless cond $ throwServer exc

guardWithM :: Member (Exc SomeException) r => ServerError -> Eff r Bool -> Eff r ()
guardWithM exc mCond = guardWith exc =<< mCond

throwServant :: Member (Exc SomeException) r => ServantErr -> Eff r a
throwServant = throwException . ServantException

throwException :: (Member (Exc SomeException) r, Exception e) => e -> Eff r a
throwException = throwError . toException

makeServantErr :: ServerError -> ServantErr
makeServantErr err = (getServantErr err) { errBody = encode err }


-- | Internal
getServantErr :: ServerError -> ServantErr
getServantErr InvalidAuthToken = err403
getServantErr InvitationAlreadyClaimed = err403
getServantErr AlreadyFamilyMember = err409
getServantErr (NoSuchFamily _) = err404
getServantErr NoSuchInvitation = err404
getServantErr NoSuchSocket = err404
getServantErr NotFound = err404
getServantErr NoDataAvailable = err404
getServantErr NoNewChannel = err404
getServantErr Forbidden = err403
getServantErr TransactionTimeout = err500

instance ToJSON ServerError where
    toJSON = genericToJSON defaultOptions


-- TODO: No longer needed, my PR making ServantErr an instance of Exception got already merged.
newtype ServantException = ServantException {
  unwrapServantErr :: ServantErr
  } deriving (Show, Typeable)

instance Exception ServantException
