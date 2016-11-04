module Gonimo.Server.Handlers.Session where

import           Control.Concurrent.STM              (STM, TVar, readTVar)
import           Control.Lens
import           Control.Monad
import           Control.Monad                       (guard)
import           Control.Monad.Extra                 (whenM)
import           Control.Monad.Freer                 (Eff)
import           Control.Monad.Freer.Reader          (ask)
import           Control.Monad.State.Class           (gets, modify)
import           Control.Monad.STM.Class             (liftSTM)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Maybe           (MaybeT (..), runMaybeT)
import qualified Data.Map.Strict                     as M
import           Data.Monoid
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy (..))
import qualified Data.Set                            as S
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Database.Persist                    (Entity (..), (==.))
import qualified Gonimo.Database.Effects             as Db
import qualified Gonimo.Database.Effects             as Db
import           Gonimo.Database.Effects.Servant     as Db
import           Gonimo.Server.Auth                  as Auth
import           Gonimo.Server.Handlers.Auth.Internal
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Error
import           Gonimo.Server.State.Session               as Session
import           Gonimo.Server.State.Types           (SessionId)

import           Control.Monad.Trans.Error           (runErrorT)
import           Gonimo.Server.Types
import           Gonimo.WebAPI                       (ReceiveChannelR,
                                                      ReceiveSocketR)
import           Gonimo.WebAPI.Types                 (InvitationInfo (..),
                                                      InvitationReply (..))
import qualified Gonimo.WebAPI.Types                 as Client
import           Servant.API                         ((:>))
import           Servant.Server                      (ServantErr (..), err400,
                                                      err403)
import           Servant.Subscriber                  (Event (ModifyEvent))
import           Utils.Control.Monad.Trans.Maybe     (maybeT)
import Database.Persist.Class

-- | A device registers itself as online.
--
--   With the returned session id a device can keep itself online by calls to statusUpdateR or
--   make itself go offline with statusDeleteR.
--
--   Only one session per device is allowed, with statusRegisterR you can steal
--   a session. Any browser tab already online (triggering statusUpdateR
--   periodically), will be set offline and the user will get an appropriate
--   error message.
statusRegisterR :: AuthServerConstraint r
                => FamilyId -> DeviceId -> DeviceType -> Eff r SessionId
statusRegisterR familyId deviceId deviceType = do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId


-- | Update status (you can change the device type any time)
--
--   Even if you don't change the device type, you have to trigger this handler
--   at least every 30 seconds in order to stay online.
--
--   If another browser tab created a new session, this handler will throw
--   Error: `SessionInvalid`, you can use this error to detect whether the
--   user opened the app in more than one browser tab.
--
--   You will get error `NoActiveSession` if there is no session at all - you
--   came too late and have to call statusRegisterR again!
statusUpdateR  :: AuthServerConstraint r
               => FamilyId -> DeviceId -> SessionId -> DeviceType -> Eff r ()
statusUpdateR familyId deviceId sessionId deviceType= do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId

    mr <- updateFamilyEff familyId . runErrorT
         $ Session.update deviceId sessionId deviceType
    maybe (return ()) handleUpdate mr
    whenM hasChanged $ do -- < No need for a single atomically here!
      _ <- updateFamilyEff familyId $ Session.update deviceId sessionId deviceType
      whenM updateLastUsedBabyNames $
        notify ModifyEvent getFamilyEndpoint (\f -> f familyId)
      notify ModifyEvent listDevicesEndpoint (\f -> f familyId)
  where
    handleUpdate :: Either Session.UpdateError DeviceType -> Eff r ()
    handleUpdate er = do
      _ <- throwLeft $ er & over _Left toServerError
      notify ModifyEvent listDevicesEndpoint (\f -> f familyId)
    updateLastUsedBabyNames = fmap (fromMaybe False) . runMaybeT $ do
        name <- toBabyName deviceType
        MaybeT . runDb . runMaybeT $ do
          oldFamily <- lift $ Db.getErr (NoSuchFamily familyId) familyId
          let oldBabies = familyLastUsedBabyNames oldFamily
          guard $ not (name `elem` oldBabies)
          let newFamily
                = oldFamily
                  { familyLastUsedBabyNames = take 5 (name : familyLastUsedBabyNames oldFamily)
                  }
          lift $ Db.replace familyId newFamily
          return True

    hasChanged = do
      state <- getState
      atomically $ do
        mFamily <- lookupFamily state familyId
        let mFound = onlineMember deviceId sessionId =<< mFamily
        return $ mFound /= Just deviceType

-- | Tell the server that you are gone.
statusDeleteR  :: AuthServerConstraint r
               => FamilyId -> DeviceId -> Eff r ()
statusDeleteR familyId deviceId = do
  authorizeAuthData $ isFamilyMember familyId
  authorizeAuthData $ isDevice deviceId
  _ <- updateFamilyEff familyId $ deleteStatus deviceId
  notify ModifyEvent listDevicesEndpoint (\f -> f familyId)

statusListDevicesR  :: AuthServerConstraint r
                    => FamilyId -> Eff r [(DeviceId, DeviceType)]
statusListDevicesR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  Session.listStatus <$> getFamilyEff familyId
