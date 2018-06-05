{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Gonimo.Server.Handlers.Auth where

import           Control.Applicative             ((<|>))
import           Control.Concurrent.STM          (STM, TVar, readTVar)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch             as X (MonadThrow (..))
import           Control.Monad.Extra             (whenM)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State.Class       (gets, modify)
import           Control.Monad.STM.Class         (liftSTM)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Identity    (IdentityT, runIdentityT)
import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT), runMaybeT)
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
import           Gonimo.Server.Config            as Eff
import qualified Gonimo.Server.Db.Account        as Account
import qualified Gonimo.Server.Db.Device         as Device
import qualified Gonimo.Server.Db.Family         as Family
import qualified Gonimo.Server.Db.Invitation     as Invitation
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Error
import           Gonimo.SocketAPI                (ServerRequest (..))
import           Gonimo.SocketAPI.Types          (Invitation (..),
                                                  InvitationDelivery (..),
                                                  InvitationId,
                                                  InvitationInfo (..),
                                                  InvitationReply (..),
                                                  InvitationSecret,
                                                  SendInvitation (..))
import           Gonimo.SocketAPI.Types          as API
import           Gonimo.Types                    (Secret)
import qualified Gonimo.Types                    as Server
import qualified Gonimo.Server.CodeInvitation    as CodeInvitation

createInvitationR :: (HasAuthData env, HasConfig env) => FamilyId -> RIO env (InvitationId, Invitation)
createInvitationR fid = do
  authorize =<< isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  -- family <- getFamily fid  -- defined but not used (yet).
  senderId' <- view authDeviceId
  let inv = Invitation {
    invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
    , invitationSenderId = senderId'
    , invitationReceiverId = Nothing
  }
  iid <- runDb $ Invitation.insert inv
  notify $ ReqGetFamilyInvitations fid
  return (iid, inv)

-- | Receive an invitation and mark it as received - it can no longer be claimed
--   by any other device.
claimInvitationR :: (HasAuthData env, HasConfig env) => Secret -> RIO env InvitationInfo
claimInvitationR invSecret = do
  aid <- view authAccountId
  (invId, invInfo) <- runDb $ do
    (invId', inv) <- Invitation.claim aid invSecret
    invFamily  <- Family.get $ invitationFamilyId inv
    invDevice  <- Device.get $ invitationSenderId inv
    mInvUser   <- Account.getUser (deviceAccountId invDevice)
    let invInfo' = InvitationInfo
                   { invitationInfoFamily = API.familyName invFamily
                   , invitationInfoSendingDevice = fromMaybe "" $ deviceName invDevice
                   , invitationInfoSendingUser = userLogin . snd <$> mInvUser
                   }
    pure (invId', invInfo')
  notify $ ReqGetInvitation invId
  pure invInfo

-- | Claim an invitation by providing a short code.
claimInvitationByCodeR :: (HasAuthData env, HasConfig env) => InvitationCode -> RIO env (InvitationSecret, InvitationInfo)
claimInvitationByCodeR invCode = do
  invId <- CodeInvitation.getIdByCode invCode
  invSecret <- fmap invitationSecret . runDb $ Invitation.get invId
  info <- claimInvitationR invSecret
  pure (invSecret, info)

-- | Actually accept or decline an invitation.
answerInvitationR :: (HasAuthData env, HasConfig env) => Secret -> InvitationReply -> RIO env (Maybe FamilyId)
answerInvitationR invSecret reply = do
  authData' <- ask
  let aid = authData' ^. authAccountId
  now <- getCurrentTime
  inv <- runDb $ do
    (iid, inv) <- Invitation.getBySecret invSecret
    guardWith InvitationAlreadyClaimed
      $ case invitationReceiverId inv of
          Nothing          -> True
          Just receiverId' -> receiverId' == aid
    Invitation.delete iid
    return inv
  predPool <- getPredicatePool

  notify $ ReqGetFamilyInvitations (invitationFamilyId inv)

  runDb $ do -- Separate transaction - we want the invitation to be deleted now!
    when (reply == InvitationAccept ) $ do
      guardWith (AlreadyFamilyMember (invitationFamilyId inv))
        $ not $ isFamilyMember (invitationFamilyId inv) authData'
      Account.joinFamily predPool $ FamilyAccount {
          familyAccountAccountId = aid
        , familyAccountFamilyId = invitationFamilyId inv
        , familyAccountJoined = now
        , familyAccountInvitedBy = Just (invitationDelivery inv)
        }
  case reply of
    InvitationAccept -> do
      notify $ ReqGetFamilies aid
      notify $ ReqGetFamilyMembers (invitationFamilyId inv)
      pure . Just $ invitationFamilyId inv
    _ -> pure Nothing


sendInvitationR :: (HasAuthData env, HasConfig env) => SendInvitation -> RIO env ()
sendInvitationR (SendInvitation iid d@(EmailInvitation email)) = do
  authData' <- ask -- Only allowed if user is member of the inviting family!
  (secret, famName, devName) <- runDb $ do
    inv <- Invitation.get iid
    authorize $ isFamilyMember (invitationFamilyId inv) authData'
    newInv <- Invitation.updateDelivery d iid
    family <- Family.get (invitationFamilyId inv)
    sendingDevice <- Device.get (invitationSenderId inv)
    let devName = fromMaybe "device with no name" $ deviceName sendingDevice
    let famName = API.familyName family
    let secret = invitationSecret newInv
    return (secret, famName, devName)

  notify $ ReqGetInvitation iid

  baseURL <- getFrontendURL
  sendEmail $ makeInvitationEmail baseURL secret email (Server.familyNameName famName) devName

sendInvitationR (SendInvitation _ OtherDelivery) = throwM CantSendInvitation

-- | Get all 'InviationId's belonging to a given family.
--
--   All members of the given family are allowed to retrieve this data.
getFamilyInvitationsR :: (HasAuthData env, HasConfig env) => FamilyId -> RIO env [InvitationId]
getFamilyInvitationsR familyId = do
  authorize =<< isFamilyMember familyId
  runDb $ map fst <$> Family.getInvitations familyId

-- | Get an invitation by its `InvitationId`.
--
--   Only members of the family this invitation belongs to are allowed to retrieve it.
getInvitationR :: (HasAuthData env, HasConfig env) => InvitationId -> RIO env Invitation
getInvitationR invId = do
  inv <- runDb $ Invitation.get invId
  authorize =<< isFamilyMember (invitationFamilyId inv)
  pure inv

createInvitationCodeR :: (HasAuthData env, HasConfig env) => InvitationId -> RIO env InvitationCode
createInvitationCodeR invId = do
  void $ getInvitationR invId -- Check that we are allowed to retrieve the invitation.
  CodeInvitation.makeCode invId


getFamilyMembersR :: (HasAuthData env, HasConfig env) => FamilyId -> RIO env [AccountId]
getFamilyMembersR familyId = do
  authorize =<< isFamilyMember familyId
  runDb $ Family.getAccountIds familyId

getDevicesR :: (HasAuthData env, HasConfig env) => AccountId -> RIO env [DeviceId]
getDevicesR accountId' = do
  (result, inFamilies) <- runDb $ do
    inFamilies' <- Account.getFamilyIds accountId'
    result' <- map fst <$> Account.getDevices accountId'
    pure (result', inFamilies')

  authorize =<< (or <$> traverse isFamilyMember inFamilies)
  pure result

getDeviceInfoR :: (HasAuthData env, HasConfig env) => DeviceId -> RIO env API.DeviceInfo
getDeviceInfoR deviceId' = do
  (result, inFamilies) <- runDb $ do
    device <- Device.get deviceId'
    inFamilies' <- Account.getFamilyIds (deviceAccountId device)
    pure (API.fromDevice device, inFamilies')

  authorize =<< (or <$> traverse isFamilyMember inFamilies)
  pure result

setDeviceNameR :: (HasAuthData env, HasConfig env) => DeviceId -> Text -> RIO env ()
setDeviceNameR deviceId' name = do
  inFamilies <- runDb $ do
    device <- Device.get deviceId'
    inFamilies' <- Account.getFamilyIds (deviceAccountId device)
    pure inFamilies'

  authorize =<< (or <$> traverse isFamilyMember inFamilies)

  _ :: Maybe () <- runDb $ runMaybeT . Device.update deviceId' $ Device.setName name
  notify $ ReqGetDeviceInfo deviceId'
  pure ()

createFamilyR :: (HasAuthData env, HasConfig env) => RIO env FamilyId
createFamilyR = do
  -- no authorization: - any valid user can create a family.
  now <- getCurrentTime
  aid <- view authAccountId
  n <- generateFamilyName
  predPool <- getPredicatePool
  fid <- runDb $ do
    fid <- Family.insert $ Family {
        API.familyName = n
      , familyCreated = now
      , familyLastAccessed = now
      , familyLastUsedBabyNames = []
    }
    Account.joinFamily predPool $ FamilyAccount {
      familyAccountAccountId = aid
      , familyAccountFamilyId = fid
      , familyAccountJoined = now
      , familyAccountInvitedBy = Nothing
    }
    return fid
  notify $ ReqGetFamilies aid
  return fid

setFamilyNameR :: (HasAuthData env, HasConfig env) => FamilyId -> Text -> RIO env ()
setFamilyNameR familyId' name = do
  authorize =<< isFamilyMember familyId'

  _ :: Maybe () <- runDb . runMaybeT $ Family.update familyId' (Family.setFamilyName name)
  notify $ ReqGetFamily familyId'

leaveFamilyR :: (HasAuthData env, HasConfig env) => AccountId -> FamilyId -> RIO env ()
leaveFamilyR accountId' familyId' = do
  -- authorizeAuthData $ isAccount accountId' -- Not needed - any member can kick another member.
  authorize =<< isFamilyMember familyId'

  runDb $ do
    Family.deleteMember accountId' familyId'
    familyAccounts <- Family.getAccountIds familyId'
    when (null familyAccounts)
      $ Family.delete familyId'
  notify $ ReqGetFamilies accountId'
  notify $ ReqGetFamilyMembers familyId'


getFamiliesR :: (HasAuthData env, HasConfig env) => AccountId -> RIO env [FamilyId]
getFamiliesR accountId' = do
  authorize =<< isAccount accountId'
  runDb $ Account.getFamilyIds accountId'

getFamilyR :: (HasAuthData env, HasConfig env) => FamilyId -> RIO env Family
getFamilyR familyId' = do
  authorize =<< isFamilyMember familyId'
  runDb $ Family.get familyId'

-- Internal function for debugging:
prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)
