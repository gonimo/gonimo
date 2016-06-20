{-# LANGUAGE RecordWildCards #-}
module Gonimo.Server.AuthHandlers where

import           Control.Monad                   (unless)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.Freer.Reader      (ask)
import           Data.Maybe                      (isJust)
import           Data.Text                       (Text)
import           Database.Persist                (Entity (..))
import qualified Gonimo.Database.Effects         as Db
import           Gonimo.Database.Effects.Servant
import           Gonimo.Server.Auth
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects
import           Gonimo.Server.EmailInvitation
import           Gonimo.Server.Types
import           Gonimo.Util
import           Servant.Server                  (ServantErr (..), err400,
                                                  err403)
import           Gonimo.WebAPI.Types as Client

createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
createInvitation fid = do
  authorize $ isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  let inv = Invitation {
    invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
  }
  iid <- runDb $ Db.insert inv
  return (iid, inv)


acceptInvitation :: AuthServerConstraint r => Secret -> Eff r Invitation
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
    return inv

sendInvitation :: AuthServerConstraint r => SendInvitation -> Eff r ()
sendInvitation (SendInvitation iid d@(EmailInvitation email)) = do
  authData <- ask -- Only allowed if user is member of the inviting family!
  (inv, family) <- runDb $ do
    inv <- get404 iid
    authorizeAuthData (isFamilyMember (invitationFamilyId inv)) authData
    let newInv = inv {
      invitationDelivery = d
    }
    Db.replace iid newInv
    family <- get404 (invitationFamilyId inv)
    return (newInv, family)
  sendEmail $ makeInvitationEmail inv email (familyName family)
sendInvitation (SendInvitation _ OtherDelivery) =
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
createChannel fid to from = do
  -- is it a good idea to expose the db-id in the route - people know how to use
  -- a packet sniffer
  AuthData{..} <- ask
  unless (fid `elem` authDataAllowedFamilies) $ throwServant err403 { errBody = "invalid family route" }
  unless (from == authDataClient) $ throwServant err403 { errBody = "from client not consistent with auth data" }
  runDb $ do client <- Db.get to
             case client of
               Nothing     -> throwServant err403 { errBody = "to client is not valid" }
               Just (Client _ toAcc' _) ->
                   do isMember <- Db.getBy (FamilyMember toAcc' fid)
                      unless (isJust isMember)  $ throwServant err403 { errBody = "to client not in the same family" }
  --secret <- generateSecret
  --putInMemory secret
  --return secret
  generateSecret


receiveChannel :: AuthServerConstraint r
               => FamilyId -> ClientId -> Eff r (ClientId, Secret)
-- | in this request @to@ is the one receiving the secret
receiveChannel fid to = do
  AuthData{..} <- ask
  from <- undefined -- actually get this from inMemory
  unless (fid `elem` authDataAllowedFamilies) $ throwServant err403 { errBody = "invalid family route" }
  unless (to == authDataClient) $ throwServant err403 { errBody = "from client not consistent with auth data" }
  runDb $ do client <- Db.get from
             case client of
               Nothing     -> throwServant err403 { errBody = "to client is not valid" }
               Just (Client _ toAcc' _) ->
                   do isMember <- Db.getBy (FamilyMember toAcc' fid)
                      unless (isJust isMember)  $ throwServant err403 { errBody = "to client not in the same family" }
  error "this secret should be fetched from memory"

putMessage :: AuthServerConstraint r
           => FamilyId -> ClientId -> ClientId -> Secret -> Text -> Eff r ()
putMessage     = undefined

receiveMessage :: AuthServerConstraint r
               => FamilyId -> ClientId -> ClientId -> Secret -> Eff r Text
receiveMessage = undefined
