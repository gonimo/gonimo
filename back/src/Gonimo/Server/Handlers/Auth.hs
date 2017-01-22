{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Gonimo.Server.Handlers.Auth where

import           Control.Applicative                  ((<|>))
import           Control.Concurrent.STM               (STM, TVar, readTVar)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra                  (whenM)
import           Control.Monad.Reader                 (ask)
import           Control.Monad.State.Class            (gets, modify)
import           Control.Monad.STM.Class              (liftSTM)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Identity         (IdentityT, runIdentityT)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       runMaybeT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.Trans.State            (StateT (..))
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid
import           Data.Proxy                           (Proxy (..))
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Database.Persist                     (Entity (..), (==.))
import           Database.Persist.Class
import qualified Database.Persist.Class               as Db
import           Gonimo.Database.Effects.Servant      as Db
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Db.Entities            as Db
import           Gonimo.Server.Effects                as Eff
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Error
import           Gonimo.Types                  as Server
import           Gonimo.SocketAPI.Types                  (InvitationInfo (..),
                                                       InvitationReply (..))
import qualified Gonimo.SocketAPI.Types                  as Client
import           Gonimo.SocketAPI (ServerRequest(..))
import           Servant.Server                       (ServantErr (..), err400,
                                                       err403)
import           Unsafe.Coerce
import           Utils.Control.Monad.Trans.Maybe      (maybeT)
import qualified Gonimo.Server.Db.Account             as Account

createInvitationR :: (AuthReader m, MonadServer m) => FamilyId -> m (InvitationId, Invitation)
createInvitationR fid = do
  authorizeAuthData $ isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  -- family <- getFamily fid  -- defined but not used (yet).
  senderId' <- authView $ deviceEntity.to entityKey
  let inv = Invitation {
    invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
    , invitationSenderId = senderId'
    , invitationReceiverId = Nothing
  }
  iid <- runDb $ Db.insert inv
  return (iid, inv)

-- | Receive an invitation and mark it as received - it can no longer be claimed
--   by any other device.
claimInvitationR :: (AuthReader m, MonadServer m) => Secret -> m InvitationInfo
claimInvitationR invSecret = do
  aid <- authView $ accountEntity . to entityKey
  runDb $ do
    Entity invId inv <- getByErr NoSuchInvitation $ SecretInvitation invSecret
    guardWithM InvitationAlreadyClaimed
      $ case invitationReceiverId inv of
        Nothing -> do
          Db.replace invId $ inv { invitationReceiverId = Just aid }
          return True
        Just receiverId' -> return $ receiverId' == aid
    invFamily  <- get404   $ invitationFamilyId inv
    invDevice  <- get404   $ invitationSenderId inv
    mInvUser   <- Db.getBy $ AccountIdUser (deviceAccountId invDevice)
    return InvitationInfo
                { invitationInfoFamily = Db.familyName invFamily
                , invitationInfoSendingDevice = fromMaybe "" $ deviceName invDevice
                , invitationInfoSendingUser = userLogin . entityVal <$> mInvUser
                }

-- | Actually accept or decline an invitation.
answerInvitationR :: (AuthReader m, MonadServer m) => Secret -> InvitationReply -> m (Maybe FamilyId)
answerInvitationR invSecret reply = do
  authData <- ask
  let aid = authData ^. accountEntity . to entityKey
  now <- getCurrentTime
  inv <- runDb $ do
    Entity iid inv <- getByErr NoSuchInvitation (SecretInvitation invSecret)
    guardWith InvitationAlreadyClaimed
      $ case invitationReceiverId inv of
          Nothing -> True
          Just receiverId' -> receiverId' == aid
    Db.delete iid
    return inv
  predPool <- getPredicatePool
  runDb $ do -- Separate transaction - we want the invitation to be deleted now!
    when (reply == InvitationAccept ) $ do
      guardWith (AlreadyFamilyMember (invitationFamilyId inv))
        $ not $ isFamilyMember (invitationFamilyId inv) authData
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


sendInvitationR :: (AuthReader m, MonadServer m) => Client.SendInvitation -> m ()
sendInvitationR (Client.SendInvitation iid d@(EmailInvitation email)) = do
  authData <- ask -- Only allowed if user is member of the inviting family!
  (inv, family) <- runDb $ do
    inv <- get404 iid
    authorize (isFamilyMember (invitationFamilyId inv)) authData
    let newInv = inv {
      invitationDelivery = d
    }
    Db.replace iid newInv
    family <- get404 (invitationFamilyId inv)
    return (newInv, family)
  baseURL <- getFrontendURL
  sendEmail $ makeInvitationEmail baseURL inv email (Db.familyName family)

sendInvitationR (Client.SendInvitation _ OtherDelivery) = throwServer CantSendInvitation

getFamilyMembersR :: (AuthReader m, MonadServer m) => FamilyId -> m [AccountId]
getFamilyMembersR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ do
    familyAccounts <- Db.selectList [FamilyAccountFamilyId ==. familyId] []
    pure $ map (familyAccountAccountId . entityVal) familyAccounts

getDevicesR :: (AuthReader m, MonadServer m) => AccountId -> m [DeviceId]
getDevicesR accountId = do
  (result, inFamilies) <- runDb $ do
    inFamilies' <- map (familyAccountFamilyId . entityVal)
                   <$> Db.selectList [ FamilyAccountAccountId ==. accountId ] []
    result' <- map entityKey <$> Db.selectList [DeviceAccountId ==. accountId] []
    pure (result', inFamilies')
  authorizeAuthData (or . \authData -> map (`isFamilyMember` authData) inFamilies)
  pure result

getDeviceInfoR :: (AuthReader m, MonadServer m) => DeviceId -> m Client.DeviceInfo
getDeviceInfoR deviceId = do
  (result, inFamilies) <- runDb $ do
    device <- Db.get404 deviceId
    inFamilies' <- map (familyAccountFamilyId . entityVal)
                   <$> Db.selectList [ FamilyAccountAccountId ==. deviceAccountId device ] []
    pure (Client.fromDevice device, inFamilies')
  -- authorizeAuthData $ or (map isFamilyMember inFamilies)
  authorizeAuthData (or . \authData -> map (`isFamilyMember` authData) inFamilies)
  pure result

createFamilyR :: (AuthReader m, MonadServer m) =>  m FamilyId
createFamilyR = do
  -- no authorization: - any valid user can create a family.
  now <- getCurrentTime
  aid <- askAccountId
  n <- generateFamilyName
  predPool <- getPredicatePool
  fid <- runDb $ do
    fid <- Db.insert Family {
        Db.familyName = n
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

leaveFamilyR :: (AuthReader m, MonadServer m) => AccountId -> FamilyId -> m ()
leaveFamilyR accountId familyId = do
  authorizeAuthData $ isAccount accountId
  authorizeAuthData $ isFamilyMember familyId

  runDb $ do
    Db.deleteBy $ Db.FamilyMember accountId familyId
    familyAccounts <- Db.selectList [ FamilyAccountFamilyId ==. familyId ] []
    when (null familyAccounts) $ do
      Db.deleteWhere [ InvitationFamilyId ==. familyId ]
      Db.delete familyId
  notify $ ReqGetFamilies accountId

getFamiliesR :: (AuthReader m, MonadServer m) => AccountId -> m [FamilyId]
getFamiliesR accountId = do
  authorizeAuthData $ isAccount accountId
  runDb $ do
    map (familyAccountFamilyId . entityVal)
      <$> Db.selectList [FamilyAccountAccountId ==. accountId] []

getFamilyR :: (AuthReader m, MonadServer m) => FamilyId -> m Family
getFamilyR familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ Db.getErr (NoSuchFamily familyId) familyId

-- Internal function for debugging:
prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)
