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
import Servant (ServantErr(..), err500, Server, (:<|>)(..), ServerT, enter, (:~>)(..))
import qualified Gonimo.Database.Effects as Db
import qualified Data.Text as T
import Servant.Server (err404, err400)
import Database.Persist (Entity(..))
import Gonimo.Server.EmailInvitation
import Control.Monad.Freer.Exception (throwError)
import Gonimo.Server.Handlers
import Gonimo.Server.AuthHandlers



effServer :: ServerT GonimoAPI ServerEffects
effServer = createAccount
        :<|> getAuthServer
        :<|> getCoffee

authServer :: ServerT AuthGonimoAPI AuthServerEffects
authServer = createInvitation
       :<|>  acceptInvitation
       :<|>  sendInvitation




-- Let's serve
server :: Server GonimoAPI
server = enter effToServant effServer

getAuthServer :: AuthToken -> ServerT AuthGonimoAPI ServerEffects
getAuthServer s = enter (authToEff s) authServerT


----------------------- Natural transformations -------------------------

authToEff' :: Maybe AuthToken -> AuthServerEffects a -> ServerEffects a
authToEff' Nothing _ = throwError err 401 { -- Not standard conform, but I don't care for now.
                          errReasonPhrase = "You need to provide an AuthToken!"
                         }
authToEff' (GonimoSecret s) m = do
    a <- runDbAuthErr $ Db.getBy $ SecretAccount s
    let authData = AuthData a
    runReader m authData
  where
    invalidAuthErr = err400 { reasonPhrase = "Your provided credentials are invalid!" }
    runDbAuthErr = servantErrOnNothing invalidAuthErr <=< runDb

authToEff :: Maybe AuthToken -> AuthServerEffects :~> ServerEffects
authToEff = Nat . authToEff'

-------

effToServant' :: ServerEffects a -> EitherT ServantErr IO a
effToServant' = EitherT $ first exceptionToServantErr <$> runExceptionServer . handleExceptions

effToServant :: ServerEffects :~> EitherT ServantErr IO
effToServant = Nat effToServant'


-------------------------------------------------------------------------


---------------------- Exception handling -------------------------------

handleExceptions :: ServerConstraint r => Eff r a -> Eff r a
handleExceptions action = catchError action $ \e ->
    case asyncExceptionFromException e of
      -- A more reliable technique for this would be: https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions .
      Just ae -> throw ae -- Async exceptions are bad - just quit.
      _ -> do
        $(logError) $ "Error: " <> show e -- For now, try to carry on for all other exceptions.
        throwError e                      -- Simply throwing an err500 to the user and log the error.



exceptionToServantErr :: SomeException -> ServantErr
exceptionToServantErr se = case fromException se of
    Just (ServantException err) -> err
    Nothing -> err500
