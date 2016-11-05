{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.Handlers.Auth where

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
import           Gonimo.Server.State
import           Gonimo.Server.State.Types
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
import Unsafe.Coerce -- For debugging - really!

createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
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
putInvitationInfo :: AuthServerConstraint r => Secret -> Eff r InvitationInfo
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
answerInvitation :: AuthServerConstraint r => Secret -> InvitationReply -> Eff r ()
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
  when (reply == InvitationAccept ) $ do
    notify ModifyEvent listFamiliesEndpoint (\f -> f aid)
    notify ModifyEvent getDeviceInfosEndpoint (\f -> f (invitationFamilyId inv))

sendInvitation :: AuthServerConstraint r => Client.SendInvitation -> Eff r ()
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

getDeviceInfos :: AuthServerConstraint r => FamilyId -> Eff r [(DeviceId, Client.DeviceInfo)]
getDeviceInfos familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ do -- TODO: After we switched to transformers - use esqueleto for this!
    familyAccounts <- Db.selectList [FamilyAccountFamilyId ==. familyId] []
    let accountIds = familyAccountAccountId . entityVal <$> familyAccounts
    let selectDevices accountId = Db.selectList [DeviceAccountId ==. accountId] []
    deviceEntities <- concat <$> traverse selectDevices accountIds
    let entityToPair (Entity key val) = (key, val)
    return $ map (fmap Client.fromDevice . entityToPair) deviceEntities

createFamily :: AuthServerConstraint r =>  FamilyName -> Eff r FamilyId
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

getAccountFamilies :: AuthServerConstraint r => AccountId -> Eff r [FamilyId]
getAccountFamilies accountId = do
  authorizeAuthData $ isAccount accountId
  runDb $ do -- TODO: After we switched to transformers - use esqueleto for this!
    map (familyAccountFamilyId . entityVal)
      <$> Db.selectList [FamilyAccountAccountId ==. accountId] []

getFamily :: AuthServerConstraint r => FamilyId -> Eff r Family
getFamily familyId = do
  authorizeAuthData $ isFamilyMember familyId
  runDb $ Db.getErr (NoSuchFamily familyId) familyId

-- | Create a channel for communication with  a baby station
--
--   The baby station must call receiveSocket within a given timeout,
--   this handler will only return a secret if the baby station did so,
--   otherwise an error is thrown (not found - `NoSuchSocket`)
createChannel :: AuthServerConstraint r
              => FamilyId -> DeviceId -> DeviceId -> Eff r Secret
createChannel familyId toId fromId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . deviceKey)

  secret <- generateSecret
  updateFamilyRetryEff SocketBusy familyId $ createChannel' secret

  notify ModifyEvent endpoint (\f -> f familyId toId)

  cleanReceivedEff NoSuchSocket familyId (channelSecrets.at toId)
  return secret
 where
   createChannel' :: Secret -> MayUpdateFamily ()
   createChannel' secret = do
     secrets <- gets _channelSecrets
     if M.member toId secrets
       then mzero
       else channelSecrets.at toId .= Just (Written (fromId, secret))
     return ()

   endpoint :: Proxy ("socket" :> ReceiveSocketR)
   endpoint = Proxy


receiveSocket :: AuthServerConstraint r
               => FamilyId -> DeviceId -> Eff r (Maybe (DeviceId, Secret))
-- in this request @to@ is the one receiving the secret
receiveSocket familyId toId = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((toId ==) . deviceKey)

    mayUpdateFamilyEff familyId $ receiveSocket'
  where
    receiveSocket' :: MayUpdateFamily (DeviceId, Secret)
    receiveSocket' = do
      secrets <- gets _channelSecrets
      val <- maybe mzero return $ secrets ^? at toId . _Just . _Written
      channelSecrets.at toId .= Just Read
      return val


putChannel :: AuthServerConstraint r
           => FamilyId -> DeviceId -> DeviceId -> Secret -> Text -> Eff r ()
putChannel familyId fromId toId secret txt = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . deviceKey)

  updateFamilyRetryEff ChannelBusy familyId $ putData txt (fromId, toId, secret)
  $logDebug $ "CHANNEL: Put data in channel " <> (T.pack . show) [prettyKey familyId, prettyKey fromId, prettyKey toId] <> ": " <> txt

  notify ModifyEvent endpoint (\f -> f familyId fromId toId secret)

  cleanReceivedEff NoSuchChannel familyId (channelData.at (fromId, toId, secret))
  where
   endpoint :: Proxy ("socket" :> ReceiveChannelR)
   endpoint = Proxy

prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)

receiveChannel :: AuthServerConstraint r
               => FamilyId -> DeviceId -> DeviceId -> Secret -> Eff r (Maybe Text)
receiveChannel familyId fromId toId secret = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . deviceKey)

  txt <- mayUpdateFamilyEff familyId $ receiveData (fromId, toId, secret)

  $logDebug $ "CHANNEL: Got data from channel " <> (T.pack . show) [ prettyKey familyId, prettyKey fromId, prettyKey toId ] <> ": " <> (T.pack . show) txt
  pure txt

