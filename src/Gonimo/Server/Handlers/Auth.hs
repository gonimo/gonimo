{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.Handlers.Auth where

import           Control.Applicative                  ((<|>))
import           Control.Concurrent.STM               (STM, TVar, readTVar)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra                  (whenM)
import           Control.Monad.State.Class            (gets, modify)
import           Control.Monad.Reader                 (ask)
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
import qualified Database.Persist.Class              as Db
import           Gonimo.Database.Effects.Servant      as Db
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Error
import           Gonimo.Server.Handlers.Auth.Internal
import           Gonimo.Server.State
import qualified Gonimo.Server.State.MessageBox       as MsgBox
import           Gonimo.Server.State.Types
import           Gonimo.Server.Types
import           Gonimo.WebAPI                        (ReceiveChannelR,
                                                       ReceiveMessageR)
import           Gonimo.WebAPI.Types                  (InvitationInfo (..),
                                                       InvitationReply (..))
import qualified Gonimo.WebAPI.Types                  as Client
import           Servant.API                          ((:>))
import           Servant.Server                       (ServantErr (..), err400,
                                                       err403)
import           Servant.Subscriber                   (Event (ModifyEvent))
import           Unsafe.Coerce
import           Utils.Control.Monad.Trans.Maybe      (maybeT)

createInvitation :: (AuthReader m, MonadServer m) => FamilyId -> m (InvitationId, Invitation)
createInvitation fid = do
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
putInvitationInfo :: (AuthReader m, MonadServer m) => Secret -> m InvitationInfo
putInvitationInfo invSecret = do
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
                { invitationInfoFamily = familyName invFamily
                , invitationInfoSendingDevice = deviceName invDevice
                , invitationInfoSendingUser = userLogin . entityVal <$> mInvUser
                }

-- | Actually accept or decline an invitation.
answerInvitation :: (AuthReader m, MonadServer m) => Secret -> InvitationReply -> m (Maybe FamilyId)
answerInvitation invSecret reply = do
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
  runDb $ do -- Separate transaction - we want the invitation to be deleted now!
    when (reply == InvitationAccept ) $ do
      guardWith AlreadyFamilyMember
        $ not $ isFamilyMember (invitationFamilyId inv) authData
      Db.insert_  FamilyAccount {
          familyAccountAccountId = aid
        , familyAccountFamilyId = invitationFamilyId inv
        , familyAccountJoined = now
        , familyAccountInvitedBy = Just (invitationDelivery inv)
        }
  case reply of
    InvitationAccept -> do
      notify ModifyEvent listFamiliesEndpoint (\f -> f aid)
      notify ModifyEvent getDeviceInfosEndpoint (\f -> f (invitationFamilyId inv))
      pure . Just $ invitationFamilyId inv
    _ -> pure Nothing


sendInvitation :: (AuthReader m, MonadServer m) => Client.SendInvitation -> m ()
sendInvitation (Client.SendInvitation iid d@(EmailInvitation email)) = do
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
  sendEmail $ makeInvitationEmail inv email (familyName family)
sendInvitation (Client.SendInvitation _ OtherDelivery) =
  throwServant err400 {
    errReasonPhrase = "OtherDelivery means - you took care of the delivery. How am I supposed to perform an 'OtherDelivery'?"
  }

getDeviceInfos :: (AuthReader m, MonadServer m) => FamilyId -> m [(DeviceId, Client.DeviceInfo)]
getDeviceInfos familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ do -- TODO: After we switched to transformers - use esqueleto for this!
    familyAccounts <- Db.selectList [FamilyAccountFamilyId ==. familyId] []
    let accountIds = familyAccountAccountId . entityVal <$> familyAccounts
    let selectDevices accountId = Db.selectList [DeviceAccountId ==. accountId] []
    deviceEntities <- concat <$> traverse selectDevices accountIds
    let entityToPair (Entity key val) = (key, val)
    return $ map (fmap Client.fromDevice . entityToPair) deviceEntities

createFamily :: (AuthReader m, MonadServer m) =>  FamilyName -> m FamilyId
createFamily n = do
  -- no authorization: - any valid user can create a family.
  now <- getCurrentTime
  aid <- askAccountId
  fid <- runDb $ do
    fid <- Db.insert Family {
        familyName = n
      , familyCreated = now
      , familyLastAccessed = now
      , familyLastUsedBabyNames = []
    }
    Db.insert_ FamilyAccount {
      familyAccountAccountId = aid
      , familyAccountFamilyId = fid
      , familyAccountJoined = now
      , familyAccountInvitedBy = Nothing
    }
    return fid
  notify ModifyEvent listFamiliesEndpoint (\f -> f aid)
  return fid

getAccountFamilies :: (AuthReader m, MonadServer m) => AccountId -> m [FamilyId]
getAccountFamilies accountId = do
  authorizeAuthData $ isAccount accountId
  runDb $ do -- TODO: After we switched to transformers - use esqueleto for this!
    map (familyAccountFamilyId . entityVal)
      <$> Db.selectList [FamilyAccountAccountId ==. accountId] []

getFamily :: (AuthReader m, MonadServer m) => FamilyId -> m Family
getFamily familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ Db.getErr (NoSuchFamily familyId) familyId

-- Internal function for debugging:
prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)
