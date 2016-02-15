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
import Control.Monad.Trans.Error (throwError)
import Control.Monad.Trans.Either (left)
import Data.Text (Text)



createAccount :: Maybe Credentials -> AuthToken
createAccount Nothing = undefined

login :: Credentials -> AuthToken
login _ = undefined

myFamilies :: AuthToken -> [FamilyId]
myFamilies _ = undefined


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

getSenders :: FamilyId -> AuthToken -> [Sender]
getSenders _ _ = undefined




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
