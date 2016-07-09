module Gonimo.Server.AuthHandlers where

import           Control.Concurrent.STM          hiding (atomically)
import           Control.Lens
import           Control.Monad                   (unless)
import           Control.Monad.Freer             (Eff)
import           Control.Monad.Freer.Reader      (ask)
import qualified Data.ByteString.Lazy.Char8      as B
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy                      (Proxy (..))
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import           Database.Persist                (Entity (..), entityKey)
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
import           Servant.Server                  (ServantErr (..), err400,
                                                  err403)
import           Servant.Subscriber              (Event (ModifyEvent))

createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
createInvitation fid = do
  authorize $ isFamilyMember fid
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
  familyMap <- atomically . readTVar =<< (_runState <$> getState)
  case fid `M.lookup` familyMap of
    Nothing -> throwServant err403 { errBody = B.intercalate "'" [ "Family " ,B.pack $ show fid
                                                                 , " invalid!"] }
    Just transientData -> do
      secret     <- generateSecret
      familyData <- atomically $ readTVar transientData
      let fromto = S.fromList [fromId,toId]
      unless (fromto `S.isSubsetOf` (familyData^.online))
             $ throwServant err403 { errBody = B.intercalate "'" [    "From " ,B.pack $ show fromId
                                                                 , ", or to " ,B.pack $ show toId
                                                                 , " invalid!"] }

      atomically $ putSecret fromId toId secret transientData
      notify ModifyEvent endpoint (\f -> f fid toId)
      return secret

 where
   endpoint :: Proxy ("socket" :> ReceiveSocketR)
   endpoint = Proxy


receiveSocket :: AuthServerConstraint r
               => FamilyId -> ClientId -> Eff r (ClientId, Secret)
-- | in this request @to@ is the one receiving the secret
receiveSocket fid toId = do
  authorize (isFamilyMember fid)
  authorize ((toId ==) . entityKey . _clientEntity)
  Just transientData <- (fid `M.lookup`) <$> (atomically . readTVar =<< (_runState <$> getState))
  _familyData <- atomically $ readTVar transientData
  fromMaybe
    (throwServant err403 { errBody = B.intercalate "'" [ "To " ,B.pack $ show toId
                                                       , " invalid!"] })
    $ return undefined -- <$> getSecret toId (familyData^.secrets)

      -- runDb $ do clientId <- Db.get fromId
      --            case clientId of
      --              Nothing     -> throwServant err403 { errBody = "from client is not valid" }
      --              Just (Client _ toAcc' _) ->
      --                  do isMember <- Db.getBy (FamilyMember toAcc' fid)
      --                     unless (isJust isMember)  $ throwServant err403 { errBody = "to client not in the same family" }


putChannel :: AuthServerConstraint r
           => FamilyId -> ClientId -> ClientId -> Secret -> Text -> Eff r ()
putChannel = undefined

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

