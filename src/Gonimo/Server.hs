module Gonimo.Server where

import Control.Monad.Freer (Eff)
import Control.Monad.Trans.Either (EitherT(..), left)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Server.Effects.TestServer
import Gonimo.Types
import Gonimo.WebAPI
import Servant (ServantErr(..), err500, Server, (:<|>)(..))
import qualified Gonimo.Database.Effects as Db
import qualified Data.Text as T
import Servant.Server (err404)



createAccount :: ServerConstraint r => Maybe Credentials -> Eff r (AccountId, AuthToken)
createAccount mcred = do
  now <- getCurrentTime
  let email = mcred >>= getUserEmail . userName
  let phone = mcred >>= getUserPhone . userName
  let password = userPassword <$> mcred
  asecret <- generateSecret
  aid <- runDb $ Db.insert $ Account {
    accountSecret = asecret
    , accountCreated = now
    , accountLastAccessed = now
    , accountEmail = email
    , accountPhone = phone
    , accountPassword = password
    }
  return (aid, GonimoSecret asecret)

login :: ServerConstraint r => Credentials -> Eff r (AccountId, AuthToken)
login _ = return undefined

myFamilies :: ServerConstraint r => AccountId -> Eff r [FamilyId]
myFamilies uid = return undefined


getInvitations :: Maybe FamilyId -> AuthToken -> [(InvitationId, Invitation)]
getInvitations _ _ = undefined

createInvitation :: FamilyId -> AuthToken -> (InvitationId, Invitation)
createInvitation _ _ = undefined

getInvitation :: InvitationId -> AuthToken -> Invitation
getInvitation _ _ = undefined

revokeInvitation :: InvitationId -> AuthToken -> ()
revokeInvitation _ _ = ()

acceptInvitation :: InvitationSecret -> AuthToken -> FamilyId
acceptInvitation _ _ = undefined

sendInvitation :: InvitationId -> InvitationDelivery -> AuthToken -> ()
sendInvitation _ _ _ = undefined

createFamily :: AuthToken -> FamilyId
createFamily _ = undefined

getCoffee :: EitherT ServantErr IO Coffee
getCoffee = left $ ServantErr { errReasonPhrase = "I am a tea pot!"
                              , errHTTPCode = 418
                              , errBody = ""
                              , errHeaders = []
                              }


runServer ::  ServerEffects a -> EitherT ServantErr IO a
runServer action = EitherT $ first errorServerToServant <$> runErrorServer action

errorServerToServant :: ServerException -> ServantErr
errorServerToServant (NotFoundException m) = err404 { errReasonPhrase = T.unpack m }
errorServerToServant (SystemException _)   = err500


-- Let's serve
server :: Server GonimoAPI
server = undefined

generateSecret :: ServerConstraint r => Eff r ByteString
generateSecret = genRandomBytes secretLength
  where secretLength = 16
