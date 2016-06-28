module Gonimo.Server.AuthHandlers where

import           Control.Concurrent.STM          (throwSTM)
import           Control.Concurrent.STM.TVar
import           Control.Lens                    hiding (from, to)
import           Control.Monad                   (unless)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.Freer.Reader      (ask)
import qualified Data.ByteString.Lazy.Char8      as B
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe, isJust)
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
import qualified Gonimo.WebAPI.Types             as Client
import           Servant.Server                  (ServantErr (..), err400,
                                                  err403)

createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
createInvitation fid = do
  authorize $ isFamilyMember fid
  now <- getCurrentTime
  isecret <- generateSecret
  family <- getFamily fid
  clientName <- authView $ clientEntity.to (clientName . entityVal)
  let inv = Invitation {
    invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
    , invitationSendingClient = clientName
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
    authorizeAuthData (isFamilyMember (invitationFamilyId inv)) authData
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
createChannel fid toId fromId = do
  -- is it a good idea to expose the db-id in the route - people know how to use
  -- a packet sniffer
  state <- _runState <$> getState
  secret <- generateSecret
  atomically $ do
      transient <- readTVar state
      let fromto = S.fromList [fromId,toId]
      unless ( fromMaybe False $ ((fromto `S.isSubsetOf`) . _online) <$> (fid `M.lookup` transient))
             $ throwSTM err403 { errBody = B.intercalate "'" ["family "    ,B.pack $ show fid
                                                             ,", or from " ,B.pack $ show fromId
                                                             ,", or to "   ,B.pack $ show toId
                                                             ," invalid!"] }
      modifyTVar' state (over (ix fid . secrets) (putSecret from to secret))
      return secret
  -- what about throwServant instead of throwSTM


receiveChannel :: AuthServerConstraint r
               => FamilyId -> ClientId -> Eff r (ClientId, Secret)
-- | in this request @to@ is the one receiving the secret
receiveChannel fid toId = do
  authData <- ask
  fromId <- error "NotYetImplemented: get from information from inmemory structure"
  unless (fid `elem` (authData^.allowedFamilies)) $ throwServant err403 { errBody = "invalid family route" }
  unless (toId == authData^.clientEntity.to entityKey) $ throwServant err403 { errBody = "from client not consistent with auth data" }
  runDb $ do client <- Db.get fromId
             case client of
               Nothing     -> throwServant err403 { errBody = "from client is not valid" }
               Just (Client _ _ toAcc' _) ->
                   do isMember <- Db.getBy (FamilyMember toAcc' fid)
                      unless (isJust isMember)  $ throwServant err403 { errBody = "to client not in the same family" }
  secret <- error "NotYetImplemented: this secret should be fetched from inmemory data structure"
  return (fromId, secret)

putMessage :: AuthServerConstraint r
           => FamilyId -> ClientId -> ClientId -> Secret -> Text -> Eff r ()
putMessage = undefined

receiveMessage :: AuthServerConstraint r
               => FamilyId -> ClientId -> ClientId -> Secret -> Eff r Text
receiveMessage = undefined


-- The following stuff should go somewhere else someday (e.g. to paradise):

-- | Get the family of the requesting device.
--
--   error 404 if not found.
--   TODO: Get this from in memory data structure when available.
getFamily :: ServerConstraint r => FamilyId -> Eff r Family
getFamily fid = runDb $ get404 fid

