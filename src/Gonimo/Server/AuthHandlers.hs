module Gonimo.Server.AuthHandlers where

import Control.Monad.Freer (Eff)
import Control.Monad.Trans.Either (EitherT(..), left)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Server.Effects.TestServer
import Gonimo.Types
import Gonimo.WebAPI
import Servant (ServantErr(..), err500, Server, (:<|>)(..), ServerT, enter, (:~>)(..), utf8Encode)
import qualified Gonimo.Database.Effects as Db
import qualified Data.Text as T
import Servant.Server (err404, err400)
import Database.Persist (Entity(..))
import Gonimo.Server.EmailInvitation
import Control.Monad.Freer.Exception (throwError)


data AuthData = AuthData {
  authDataAccountEntity :: Entity Account
}

type AuthServerConstraint r = (Member (Reader AuthData) r, ServerConstraint r)
type AuthServerEffects = Eff '[Reader (Entity Account), Exc ServerException, Server]


createInvitation :: AuthServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
createInvitation fid = do
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
  now <- getCurrentTime
  uid <- error "TODO"
  runDb $ do
    Entity iid inv <- Db.getBy404 (SecretInvitation invSecret)
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
  (inv, family) <- runDb $ do
    inv <- Db.get iid
    let newInv = inv {
      invitationDelivery = d
    }
    Db.replace iid newInv
    family <- Db.get (invitationFamilyId inv)
    return (newInv, family)
  sendEmail $ makeInvitationEmail inv email (familyName family)
sendInvitation (SendInvitation _ OtherDelivery) =
  throwError $ BadRequest "OtherDelivery means - you took care of the delivery. How am I supposed to perform an 'OtherDelivery'?"


createFamily :: AuthServerConstraint r => FamilyName -> Eff r FamilyId
createFamily n = do
  now <- getCurrentTime
  uid <- error "TODO"
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
