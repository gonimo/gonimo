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
import qualified Gonimo.Server.Db.Family as Family

-- | A device registers itself as online.
--
--   With the returned session id a device can keep itself online by calls to sessionUpdateR or
--   make itself go offline with sessionDeleteR.
--
--   Only one session per device is allowed, with sessionRegisterR you can steal
--   a session. Any browser tab already online (triggering sessionUpdateR
--   periodically), will be set offline and the user will get an appropriate
--   error message.
sessionRegisterR :: AuthServerConstraint r
                => FamilyId -> DeviceId -> DeviceType -> Eff r SessionId
sessionRegisterR familyId deviceId deviceType = do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId

    sessionId <- updateFamilyEff familyId $ Session.register deviceId deviceType
    -- Notify + update lastseen timestamp + update last used baby names!
    pure sessionId

-- | Update session (you can change the device type any time)
--
--   Even if you don't change the device type, you have to trigger this handler
--   at least every 30 seconds in order to stay online.
--
--   If another browser tab created a new session, this handler will throw
--   Error: `SessionInvalid`, you can use this error to detect whether the
--   user opened the app in more than one browser tab.
--
--   You will get error `NoActiveSession` if there is no session at all - you
--   came too late and have to call sessionRegisterR again!
sessionUpdateR  :: AuthServerConstraint r
               => FamilyId -> DeviceId -> SessionId -> DeviceType -> Eff r ()
sessionUpdateR familyId deviceId sessionId deviceType= do
    authorizeAuthData $ isFamilyMember familyId
    authorizeAuthData $ isDevice deviceId

    mr <- mayUpdateFamilyEff familyId . runErrorT
         $ Session.update deviceId sessionId deviceType
    maybe (return ()) handleUpdate mr
  where
    handleUpdate :: Either Session.UpdateError DeviceType -> Eff r ()
    handleUpdate er = do
      _ <- throwLeft $ er & over _Left toServerError
      mNotifyFamily <- runDb . runMaybeT $ do
        name <- toBabyName deviceType
        _ <- lift $ Family.update familyId (Family.pushBabyName name)
        pure $ notify ModifyEvent getFamilyEndpoint (\f -> f familyId)
      sequence_ mNotifyFamily
      notify ModifyEvent listDevicesEndpoint (\f -> f familyId)

-- | Tell the server that you are gone.
sessionDeleteR  :: AuthServerConstraint r
               => FamilyId -> DeviceId -> Eff r ()
sessionDeleteR familyId deviceId = do
  authorizeAuthData $ isFamilyMember familyId
  authorizeAuthData $ isDevice deviceId
  _ <- updateFamilyEff familyId $ Session.delete deviceId
  notify ModifyEvent listDevicesEndpoint (\f -> f familyId)

sessionListDevicesR  :: AuthServerConstraint r
                    => FamilyId -> Eff r [(DeviceId, DeviceType)]
sessionListDevicesR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  Session.list <$> getFamilyEff familyId
