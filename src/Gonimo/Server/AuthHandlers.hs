{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Server.AuthHandlers where

import           Control.Concurrent.STM          hiding (atomically)
import           Control.Lens
import           Control.Monad.Freer             (Eff)
import           Control.Monad.Freer.Reader      (ask)
import qualified Data.Map.Strict                 as M
import           Data.Proxy                      (Proxy (..))
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import           Database.Persist                (Entity (..))
import qualified Gonimo.Database.Effects         as Db
import           Gonimo.Database.Effects.Servant
import           Gonimo.Server.Auth              as Auth
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.State
import           Gonimo.Server.Types
import           Gonimo.Util
import           Gonimo.WebAPI                   (ReceiveSocketR)
import qualified Gonimo.WebAPI.Types             as Client
import           Servant.API                     ((:>))
import           Servant.Server                  (ServantErr (..), err400)
import           Servant.Subscriber              (Event (ModifyEvent))

createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
createInvitation fid = do
  authorizeAuthData $ isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  family <- getFamily fid
  clientName' <- authView $ clientEntity.to (clientName . entityVal)
  let inv = Invitation {
    invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
    , invitationSendingClient = clientName'
    , invitationSendingFamily = familyName family
    , invitationSendingUser = Nothing
  }
  iid <- runDb $ Db.insert inv
  return (iid, inv)


getInvitation :: AuthServerConstraint r => Secret -> Eff r Invitation
getInvitation invSecret = runDb $ entityVal <$> getBy404 (SecretInvitation invSecret)

acceptInvitation :: AuthServerConstraint r => Secret -> Eff r ()
acceptInvitation invSecret = do
  -- no authorization: valid user, secret can be found - all fine.
  now <- getCurrentTime
  uid <- askAccountId
  runDb $ do
    Entity iid inv <- getBy404 (SecretInvitation invSecret)
    Db.delete iid
    Db.insert_  FamilyAccount {
        familyAccountAccountId = uid
      , familyAccountFamilyId = invitationFamilyId inv
      , familyAccountJoined = now
      , familyAccountInvitedBy = Just (invitationDelivery inv)
    }
    return ()

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


createFamily :: AuthServerConstraint r =>  FamilyName -> Eff r FamilyId
createFamily n = do
  -- no authorization: - any valid user can create a family.
  now <- getCurrentTime
  uid <- askAccountId
  runDb $ do
    fid <- Db.insert Family {
        familyName = n
      , familyCreated = now
      , familyLastAccessed = now
    }
    Db.insert_ FamilyAccount {
      familyAccountAccountId = uid
      , familyAccountFamilyId = fid
      , familyAccountJoined = now
      , familyAccountInvitedBy = Nothing
    }
    return fid

createChannel :: AuthServerConstraint r
              => FamilyId -> ClientId -> ClientId -> Eff r Secret
createChannel familyId toId fromId = do
  -- is it a good idea to expose the db-id in the route - people know how to use
  -- a packet sniffer
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . clientKey)

  familyOnlineData  <- authorizeJust (familyId `M.lookup`)
                    =<< atomically . readTVar
                    =<< getState

  secret            <- generateSecret
  familyOnlineState <- atomically $ readTVar familyOnlineData
  let fromto = S.fromList [fromId, toId]

  authorize (fromto `S.isSubsetOf`) (familyOnlineState^.onlineMembers)

  atomically $ putSecret fromId toId secret familyOnlineData
  notify ModifyEvent endpoint (\f -> f familyId toId)
  return secret

 where
   endpoint :: Proxy ("socket" :> ReceiveSocketR)
   endpoint = Proxy


receiveSocket :: AuthServerConstraint r
               => FamilyId -> ClientId -> Eff r (ClientId, Secret)
-- | in this request @to@ is the one receiving the secret
receiveSocket familyId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . clientKey)

  familyOnlineData  <- authorizeJust (familyId `M.lookup`)
                    =<< atomically . readTVar
                    =<< getState

  authorizeJust id =<< atomically (receieveSecret toId familyOnlineData)


putChannel :: AuthServerConstraint r
           => FamilyId -> ClientId -> ClientId -> Secret -> Text -> Eff r ()
putChannel familyId fromId toId token txt = undefined

receiveChannel :: AuthServerConstraint r
               => FamilyId -> ClientId -> ClientId -> Secret -> Eff r Text
receiveChannel = undefined


-- The following stuff should go somewhere else someday (e.g. to paradise):

-- | Get the family of the requesting device.
--
--   error 404 if not found.
--   TODO: Get this from in memory data structure when available.
getFamily :: ServerConstraint r => FamilyId -> Eff r Family
getFamily fid = runDb $ get404 fid

