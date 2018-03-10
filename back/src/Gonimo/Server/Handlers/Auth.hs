{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Gonimo.Server.Handlers.Auth where

import           Control.Applicative             ((<|>))
import           Control.Concurrent.STM          (STM, TVar, readTVar)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra             (whenM)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State.Class       (gets, modify)
import           Control.Monad.STM.Class         (liftSTM)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Identity    (IdentityT, runIdentityT)
import           Control.Monad.Trans.Maybe       (MaybeT (..), runMaybeT)
import           Control.Monad.Trans.State       (StateT (..))
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import           Data.Proxy                      (Proxy (..))
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Persist                (Entity (..), (==.))
import           Database.Persist.Class
import qualified Database.Persist.Class          as Db
import           Unsafe.Coerce
import           Utils.Control.Monad.Trans.Maybe (maybeT)

import           Gonimo.Database.Effects.Servant as Db
import           Gonimo.Server.Auth              as Auth
import qualified Gonimo.Server.Db.Account        as Account
import qualified Gonimo.Server.Db.Device         as Device
import qualified Gonimo.Server.Db.Family         as Family
import qualified Gonimo.Server.Db.Invitation     as Invitation
import           Gonimo.Server.Effects           as Eff
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Error
import           Gonimo.SocketAPI                (ServerRequest (..))
import           Gonimo.SocketAPI.Types          (InvitationInfo (..),
                                                  InvitationReply (..))
import           Gonimo.SocketAPI.Types          as API
import           Gonimo.Types.Extended           (InvitationDelivery (..),
                                                  Secret)
import qualified Gonimo.Types.Extended           as Server

createInvitationR :: (AuthReader m, MonadServer m) => FamilyId -> m (InvitationId, Invitation)
createInvitationR fid = do
  authorizeAuthData $ isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  -- family <- getFamily fid  -- defined but not used (yet).
  senderId' <- authView authDeviceId
  let inv = Invitation { invitationSecret     = isecret
                       , invitationFamilyId   = fid
                       , invitationCreated    = now
                       , invitationDelivery   = OtherDelivery
                       , invitationSenderId   = senderId'
                       , invitationReceiverId = Nothing
                       }
  iid <- runDb $ Invitation.insert inv
  return (iid, inv)

-- | Receive an invitation and mark it as received - it can no longer be claimed
--   by any other device.
claimInvitationR :: (AuthReader m, MonadServer m) => Secret -> m InvitationInfo
claimInvitationR invSecret = do
  aid <- authView authAccountId
  runDb $ do
    (_, inv) <- Invitation.claim aid invSecret
    invFamily  <- Family.get $ invitationFamilyId inv
    invDevice  <- Device.get $ invitationSenderId inv
    mInvUser   <- Account.getUser (deviceAccountId invDevice)
    return InvitationInfo
                { invitationInfoFamily        = API.familyName invFamily
                , invitationInfoSendingDevice = fromMaybe "" $ deviceName invDevice
                , invitationInfoSendingUser   = userLogin . snd <$> mInvUser
                }

-- | Actually accept or decline an invitation.
answerInvitationR :: (AuthReader m, MonadServer m) => Secret -> InvitationReply -> m (Maybe FamilyId)
answerInvitationR invSecret reply = do
  authData <- ask
  let aid = authData ^. authAccountId
  now <- getCurrentTime
  inv <- runDb $ do
    (iid, inv) <- Invitation.getBySecret invSecret
    guardWith InvitationAlreadyClaimed
      $ case invitationReceiverId inv of
          Nothing -> True
          Just receiverId' -> receiverId' == aid
    Invitation.delete iid
    return inv
  predPool <- getPredicatePool
  runDb $ -- Separate transaction - we want the invitation to be deleted now!
    when (reply == InvitationAccept ) $ do
      guardWith (AlreadyFamilyMember (invitationFamilyId inv))
        $ not $ isFamilyMember (invitationFamilyId inv) authData
      Account.joinFamily predPool
        FamilyAccount { familyAccountAccountId = aid
                      , familyAccountFamilyId  = invitationFamilyId inv
                      , familyAccountJoined    = now
                      , familyAccountInvitedBy = Just (invitationDelivery inv)
                      }
  case reply of
    InvitationAccept -> do
      notify $ ReqGetFamilies aid
      notify $ ReqGetFamilyMembers (invitationFamilyId inv)
      pure . Just $ invitationFamilyId inv
    _ -> pure Nothing


sendInvitationR :: (AuthReader m, MonadServer m) => API.SendInvitation -> m ()
sendInvitationR (API.SendInvitation iid d@(EmailInvitation email)) = do
  authData <- ask -- Only allowed if user is member of the inviting family!
  (secret, famName, devName) <- runDb $ do
    inv <- Invitation.get iid
    authorize (isFamilyMember (invitationFamilyId inv)) authData
    newInv <- Invitation.updateDelivery d iid
    family <- Family.get (invitationFamilyId inv)
    sendingDevice <- Device.get (invitationSenderId inv)
    let devName = fromMaybe "device with no name" $ deviceName sendingDevice
    let famName = API.familyName family
    let secret = API.invitationSecret newInv
    return (secret, famName, devName)
  baseURL <- getFrontendURL
  sendEmail $ makeInvitationEmail baseURL secret email (Server.familyNameName famName) devName

sendInvitationR (API.SendInvitation _ OtherDelivery) = throwServer CantSendInvitation

getFamilyMembersR :: (AuthReader m, MonadServer m) => FamilyId -> m [AccountId]
getFamilyMembersR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ Family.getAccountIds familyId

getDevicesR :: (AuthReader m, MonadServer m) => AccountId -> m [DeviceId]
getDevicesR accountId' = do
  (result, inFamilies) <- runDb $ do
    inFamilies' <- Account.getFamilyIds accountId'
    result' <- map fst <$> Account.getDevices accountId'
    pure (result', inFamilies')
  authorizeAuthData (or . \authData -> map (`isFamilyMember` authData) inFamilies)
  pure result

getDeviceInfoR :: (AuthReader m, MonadServer m) => DeviceId -> m API.DeviceInfo
getDeviceInfoR deviceId' = do
  (result, inFamilies) <- runDb $ do
    device <- Device.get deviceId'
    inFamilies' <- Account.getFamilyIds (deviceAccountId device)
    pure (API.fromDevice device, inFamilies')
  -- authorizeAuthData $ or (map isFamilyMember inFamilies)
  authorizeAuthData (or . \authData -> map (`isFamilyMember` authData) inFamilies)
  pure result

setDeviceNameR :: (AuthReader m, MonadServer m) => DeviceId -> Text -> m ()
setDeviceNameR deviceId' name = do
  inFamilies <- runDb $ do
    device <- Device.get deviceId'
    inFamilies' <- Account.getFamilyIds (deviceAccountId device)
    pure inFamilies'
  -- authorizeAuthData $ or (map isFamilyMember inFamilies)
  authorizeAuthData (or . \authData -> map (`isFamilyMember` authData) inFamilies)

  _ :: Maybe () <- runDb $ runMaybeT . Device.update deviceId' $ Device.setName name
  notify $ ReqGetDeviceInfo deviceId'
  pure ()

createFamilyR :: (AuthReader m, MonadServer m) =>  m FamilyId
createFamilyR = do
  -- no authorization: - any valid user can create a family.
  now <- getCurrentTime
  aid <- askAccountId
  n <- generateFamilyName
  predPool <- getPredicatePool
  fid <- runDb $ do
    fid <- Family.insert
             Family { API.familyName          = n
                    , familyCreated           = now
                    , familyLastAccessed      = now
                    , familyLastUsedBabyNames = []
                    }
    Account.joinFamily predPool
      FamilyAccount { familyAccountAccountId = aid
                    , familyAccountFamilyId  = fid
                    , familyAccountJoined    = now
                    , familyAccountInvitedBy = Nothing
                    }
    pure fid
  notify $ ReqGetFamilies aid
  pure fid

setFamilyNameR :: (AuthReader m, MonadServer m) => FamilyId -> Text -> m ()
setFamilyNameR familyId' name = do
  authorizeAuthData $ isFamilyMember familyId'

  _ :: Maybe () <- runDb . runMaybeT $ Family.update familyId' (Family.setFamilyName name)
  notify $ ReqGetFamily familyId'

leaveFamilyR :: (AuthReader m, MonadServer m) => AccountId -> FamilyId -> m ()
leaveFamilyR accountId' familyId' = do
  -- authorizeAuthData $ isAccount accountId' -- Not needed - any member can kick another member.
  authorizeAuthData $ isFamilyMember familyId'

  runDb $ do
    Family.deleteMember accountId' familyId'
    familyAccounts <- Family.getAccountIds familyId'
    when (null familyAccounts)
      $ Family.delete familyId'
  notify $ ReqGetFamilies accountId'
  notify $ ReqGetFamilyMembers familyId'


getFamiliesR :: (AuthReader m, MonadServer m) => AccountId -> m [FamilyId]
getFamiliesR accountId' = do
  authorizeAuthData $ isAccount accountId'
  runDb $ Account.getFamilyIds accountId'

getFamilyR :: (AuthReader m, MonadServer m) => FamilyId -> m Family
getFamilyR familyId' = do
  authorizeAuthData $ isFamilyMember familyId'
  runDb $ Family.get familyId'

-- Internal function for debugging:
prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)
