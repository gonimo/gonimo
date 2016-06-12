{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Except (ExceptT(..))
import Data.Aeson (encode)
import Data.Bifunctor (first)
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Server.Effects.TestServer
import Gonimo.Server.Types
import Gonimo.WebAPI
import Servant (ServantErr(..), err500, Server, (:<|>)(..), ServerT, enter, (:~>)(..))
import qualified Gonimo.Database.Effects as Db
import qualified Data.Text as T
import Servant.Server (err400, err401)
import Database.Persist (Entity(..), (==.))
import Control.Monad.Freer.Exception (throwError, catchError)
import Gonimo.Server.Handlers
import Gonimo.Server.AuthHandlers
import Gonimo.Server.Auth
import Gonimo.Database.Effects.Servant
import Control.Monad ((<=<))
import Control.Exception (asyncExceptionFromException, throw, SomeException, fromException, AsyncException)
import Data.Monoid
import Gonimo.Util (ServantException(..), throwServant)
import Gonimo.Error
import Servant.Utils.StaticFiles (serveDirectory)


getDevelopmentServer :: Config -> Server DevelopmentAPI
getDevelopmentServer c = getServer c
  :<|> serveDirectory "/home/robert/projects/gonimo-front/dist"

effServer :: ServerT GonimoAPI ServerEffects
effServer =  createClient
        :<|> getAuthServer
        :<|> getCoffee

authServer :: ServerT AuthGonimoAPI AuthServerEffects
authServer = createInvitation
        :<|> acceptInvitation
        :<|> sendInvitation
        :<|> createFamily
        :<|> createChannel
        :<|> receiveChannel
        :<|> putMessage
        :<|> receiveMessage


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
authToEff' (Just s) m = do
    authData <- runDb $ do
      Entity cid Client{..} <- getByAuthErr $ AuthTokenClient s
      account <- getAuthErr clientAccountId
      fids <- map (familyAccountFamilyId . entityVal) <$> Db.selectList [FamilyAccountAccountId ==. clientAccountId] []
      return $ AuthData (Entity clientAccountId account) fids cid
    runReader m authData
  where
    invalidAuthErr = err400 { errReasonPhrase = "The provided AuthToken is not valid!"
      , errBody = encode InvalidAuthToken
      }
    getByAuthErr = servantErrOnNothing invalidAuthErr <=< Db.getBy
    getAuthErr = servantErrOnNothing invalidAuthErr <=< Db.get

authToEff :: Maybe AuthToken -> AuthServerEffects :~> ServerEffects
authToEff token = Nat $ authToEff' token

-------

effToServant' :: Config -> ServerEffects a -> ExceptT ServantErr IO a
effToServant' c = ExceptT . fmap (first exceptionToServantErr) . runExceptionServer c . handleExceptions

effToServant :: Config -> ServerEffects :~> ExceptT ServantErr IO
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
