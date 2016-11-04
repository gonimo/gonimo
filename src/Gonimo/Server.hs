{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Gonimo.Server where

import           Control.Exception               (AsyncException, SomeException,
                                                  asyncExceptionFromException,
                                                  fromException, throw)
import           Control.Monad                   ((<=<))
import           Control.Monad.Except            (ExceptT (..))
import           Control.Monad.Freer             (Eff)
import           Control.Monad.Freer.Exception   (catchError, throwError)
import           Control.Monad.Freer.Reader      (runReader)
import           Data.Bifunctor                  (first)
import           Data.Monoid
import qualified Data.Text                       as T
import           Database.Persist                (Entity (..), (==.))
import qualified Gonimo.Database.Effects         as Db
import           Gonimo.Database.Effects.Servant
import           Gonimo.Server.Auth
import           Gonimo.Server.Handlers.Auth
import           Gonimo.Server.Handlers.Session
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects           hiding (Server)
import           Gonimo.Server.Error    (ServantException (..), throwServant, ServerError (..))
#ifdef DEVELOPMENT
import           Gonimo.Server.Effects.Development
#else
import           Gonimo.Server.Effects.Production
#endif
import           Gonimo.Server.Handlers
import           Gonimo.Server.Types
import           Gonimo.WebAPI
import           Servant                ((:<|>) (..), (:~>) (..),
                                         ServantErr (..), Server, ServerT,
                                         enter, err500)
import           Servant.Server         (err401)


effServer :: ServerT GonimoAPI ServerEffects
effServer =  createDevice
        :<|> getAuthServer
        :<|> createFunnyName
        :<|> getCoffee

authServer :: ServerT AuthGonimoAPI AuthServerEffects
authServer = invitationsServer
        :<|> accountsServer
        :<|> familiesServer
        :<|> socketServer
        :<|> statusServer

invitationsServer :: ServerT InvitationsAPI AuthServerEffects
invitationsServer = createInvitation
               :<|> answerInvitation
               :<|> sendInvitation
               :<|> putInvitationInfo

accountsServer :: ServerT AccountsAPI AuthServerEffects
accountsServer = getAccountFamilies

familiesServer :: ServerT FamiliesAPI AuthServerEffects
familiesServer = createFamily
            :<|> familyServer

familyServer :: FamilyId -> ServerT FamilyAPI AuthServerEffects
familyServer familyId = getFamily familyId
                   :<|> getDeviceInfos familyId

socketServer :: ServerT SocketAPI AuthServerEffects
socketServer = createChannel
          :<|> receiveSocket
          :<|> putChannel
          :<|> receiveChannel

statusServer :: ServerT StatusAPI AuthServerEffects
statusServer = statusRegisterR
          :<|> statusUpdateR
          :<|> statusDeleteR
          :<|> statusListDevicesR


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
      device@(Entity _ Device{..}) <- getByAuthErr $ AuthTokenDevice s
      account <- getAuthErr deviceAccountId
      fids <- map (familyAccountFamilyId . entityVal)
              <$> Db.selectList [FamilyAccountAccountId ==. deviceAccountId] []
      return $ AuthData (Entity deviceAccountId account) fids device
    runReader m authData
  where
    getByAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.getBy
    getAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.get

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

