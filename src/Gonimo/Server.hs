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
import           Servant (ServantErr, err500, Server)




sendInvitation :: FamilyId -> Invitation -> EitherT ServantErr IO ()
sendInvitation _ (EmailInvitation addr) = runServer $ S.sendInvitation (Address Nothing addr)

runServer ::  ServerEffects a -> EitherT ServantErr IO a
runServer action = EitherT $ first errorServerToServant <$> runErrorServer action 

errorServerToServant :: ServerError -> ServantErr
errorServerToServant (SystemException e) = err500


-- Let's serve
server :: Server GonimoAPI
server = sendInvitation
