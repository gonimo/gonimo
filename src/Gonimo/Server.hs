module Gonimo.Server where

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



createAccount :: ServerConstraint r => Maybe Credentials -> Eff r (AccountId, AuthToken)
createAccount mcred = do
  now <- getCurrentTime
  let email = mcred >>= getUserEmail . userName
  let phone = mcred >>= getUserPhone . userName
  let password = userPassword <$> mcred
  asecret <- generateSecret
  aid <- runDb $ Db.insert Account {
    accountSecret = asecret
    , accountCreated = now
    , accountLastAccessed = now
    , accountEmail = email
    , accountPhone = phone
    , accountPassword = password
    }
  return (aid, GonimoSecret asecret)

createInvitation :: ServerConstraint r => FamilyId -> Eff r (InvitationId, Invitation)
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


acceptInvitation :: ServerConstraint r => Secret -> Eff r Invitation
acceptInvitation invSecret = do
  now <- getCurrentTime
  uid <- error "TODO"
  runDb $ do
    Entity iid inv <- Db.getBy (SecretInvitation invSecret)
    Db.delete iid
    Db.insert_  FamilyAccount {
        familyAccountAccountId = uid
      , familyAccountFamilyId = invitationFamilyId inv
      , familyAccountJoined = now
      , familyAccountInvitedBy = Just (invitationDelivery inv)
    }
    return inv

sendInvitation :: ServerConstraint r => SendInvitation -> Eff r ()
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


createFamily :: ServerConstraint r => FamilyName -> Eff r FamilyId
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

getCoffee :: ServerConstraint r => Eff r Coffee
getCoffee = throwError TeaPot

-- runServer ::  ServerEffects a -> EitherT ServantErr IO a
-- runServer action = EitherT $ first errorServerToServant <$> runErrorServer action

errorServerToServant :: ServerException -> ServantErr
errorServerToServant NotFound              = err404
errorServerToServant (BadRequest m)        = err400 {errBody = encodeUtf8 m}
errorServerToServant TeaPot                = ServantErr { errReasonPhrase = "I am a tea pot!"
                                                        , errHTTPCode = 418
                                                        , errBody = ""
                                                        , errHeaders = []
                                                        }
errorServerToServant (SystemException _)   = err500


authServerT :: AuthToken -> ServerT AuthGonimoAPI ServerEffects
authServerT token = undefined

effServerT :: ServerT GonimoAPI ServerEffects
effServerT = createAccount
        :<|> authServerT
        :<|> getCoffee

-- Let's serve
server :: Server GonimoAPI
server = enter effToServantT effServerT

effToServantT' :: ServerEffects a -> EitherT ServantErr IO a
effToServantT' = EitherT $ first errorServerToServant <$> runExceptionServer

effToServantT :: ServerEffects :~> EitherT ServantErr IO
effToServantT = Nat effToServantT'

generateSecret :: ServerConstraint r => Eff r Secret
generateSecret = Secret <$> genRandomBytes secretLength
  where secretLength = 16
