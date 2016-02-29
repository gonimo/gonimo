{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (runReader)
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
import Servant.Server (err404, err400, err401)
import Database.Persist (Entity(..), (==.))
import Gonimo.Server.EmailInvitation
import Control.Monad.Freer.Exception (throwError, catchError)
import Gonimo.Server.Handlers
import Gonimo.Server.AuthHandlers
import Gonimo.Server.Auth
import Gonimo.Database.Effects.Servant
import Control.Monad ((<=<))
import Control.Exception (asyncExceptionFromException, throw, SomeException, fromException, AsyncException)
import Data.Monoid
import Gonimo.Util (ServantException(..), throwServant)



effServer :: ServerT GonimoAPI ServerEffects
effServer = createAccount
        :<|> getAuthServer
        :<|> getCoffee

authServer :: ServerT AuthGonimoAPI AuthServerEffects
authServer = createInvitation
       :<|>  acceptInvitation
       :<|>  sendInvitation
       :<|>  createFamily


-- Let's serve
getServer :: Config -> Server GonimoAPI
getServer c = enter (effToServant c) effServer

getAuthServer :: Maybe AuthToken -> ServerT AuthGonimoAPI ServerEffects
getAuthServer s = enter (authToEff s) authServer


----------------------- Natural transformations -------------------------

authToEff' :: Maybe AuthToken -> AuthServerEffects a -> ServerEffects a
authToEff' Nothing _ = throwServant err401 { -- Not standard conform, but I don't care for now.
                          errReasonPhrase = "You need to provide an AuthToken!"
                         }
authToEff' (Just (GonimoSecret s)) m = do
    authData <- runDb $ do
      ae@(Entity aid _) <- getByAuthErr $ SecretAccount s
      fids <- map (familyAccountFamilyId . entityVal) <$> Db.selectList [FamilyAccountAccountId ==. aid] []
      return $ AuthData ae fids
    runReader m authData
  where
    invalidAuthErr = err400 { errReasonPhrase = "Your provided credentials are invalid!" }
    getByAuthErr = servantErrOnNothing invalidAuthErr <=< Db.getBy

authToEff :: Maybe AuthToken -> AuthServerEffects :~> ServerEffects
authToEff token = Nat $ authToEff' token

-------

effToServant' :: Config -> ServerEffects a -> EitherT ServantErr IO a
effToServant' c = EitherT . fmap (first exceptionToServantErr) . runExceptionServer c . handleExceptions

effToServant :: Config -> ServerEffects :~> EitherT ServantErr IO
effToServant c = Nat $ effToServant' c


-------------------------------------------------------------------------


---------------------- Exception handling -------------------------------

handleExceptions :: ServerConstraint r => Eff r a -> Eff r a
handleExceptions action = catchError action $ \e ->
    case asyncExceptionFromException e of
      -- A more reliable technique for this would be: https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions .
      Just (ae :: AsyncException) -> throw ae -- Async exceptions are bad - just quit.
      _ -> do
        $(logError) $ "Error: " <> T.pack (show e) -- For now, try to carry on for all other exceptions.
        throwError e                      -- Simply throwing an err500 to the user and log the error.



exceptionToServantErr :: SomeException -> ServantErr
exceptionToServantErr se = case fromException se of
    Just (ServantException err) -> err
    Nothing -> err500
