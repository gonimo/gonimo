module Gonimo.Server.AuthHandlers where

import Data.Text (Text)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (ask)
import Gonimo.Server.DbTypes
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects
import Gonimo.Database.Effects.Servant
import Gonimo.Types
import qualified Gonimo.Database.Effects as Db
import Servant.Server (err400, ServantErr(..))
import Database.Persist (Entity(..))
import Gonimo.Server.EmailInvitation
import Gonimo.Server.Auth
import Gonimo.Util

-- We have to use AuthServerEffects instead of constraint AuthServerConstraint, as an open
-- constraint does not seem to play well with servant. (Ambiguous type errors)
createInvitation :: FamilyId -> AuthServerEffects (InvitationId, Invitation)
-- createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
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


acceptInvitation :: Secret -> AuthServerEffects Invitation
-- acceptInvitation :: AuthServerConstraint r => Secret -> Eff r Invitation
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

sendInvitation :: SendInvitation -> AuthServerEffects ()
-- sendInvitation :: AuthServerConstraint r => Maybe SendInvitation -> Eff r ()
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


createFamily :: Text -> AuthServerEffects FamilyId
-- createFamily :: AuthServerConstraint r =>  Maybe FamilyName -> Eff r FamilyId
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
