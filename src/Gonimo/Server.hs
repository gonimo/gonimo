module Gonimo.Server where

import           Control.Monad.Freer (Eff)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.Bifunctor (first)
import           Gonimo.Server.Effects hiding (Server)
import           Gonimo.Server.Effects.TestServer
import qualified Gonimo.Server.EmailInvitation as S
import           Gonimo.WebAPI
import           Network.Mail.Mime (Address(..))
import           Servant (ServantErr(..), err500, Server, (:<|>)(..))
import Servant.Server (err401)
import Control.Monad.Trans.Either (left)
import Data.Text (Text)

import Gonimo.Types
import Gonimo.Server.DbTypes
import Gonimo.Server.DbEntities
import Data.ByteString (ByteString)



createAccount :: ServerConstraint r => Maybe Credentials -> Eff r (AccountId, AuthToken)
createAccount mcred = do
  now <- getCurrentTime
  let email = mcred >>= getUserEmail . userName 
  let phone = mcred >>= getUserPhone . userName 
  let password = userPassword <$> mcred
  secret <- generateSecret
  id <- insertDb $ Account {
    accountSecret = secret
    , accountCreated = now
    , accountLastAccessed = now
    , accountEmail = email
    , accountPhone = phone
    , accountPassword = password
    }
  return (id, GonimoSecret secret)
    

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
                              }


runServer ::  ServerEffects a -> EitherT ServantErr IO a
runServer action = EitherT $ first errorServerToServant <$> runErrorServer action 

errorServerToServant :: ServerError -> ServantErr
errorServerToServant (SystemException e) = err500

authServer :: Maybe AuthToken -> Server AuthGonimoAPI
-- authServer Nothing = left $ err401 { errReasonPhrase = "" }
authServer = undefined

-- Let's serve
server :: Server GonimoAPI
server = undefined

generateSecret :: ServerConstraint r => Eff r ByteString
generateSecret = genRandomBytes secretLength
  where secretLength = 16
