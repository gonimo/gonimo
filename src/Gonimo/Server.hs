{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Gonimo.Server where

import           Control.Monad                   ((<=<))
import           Control.Monad.Except            (ExceptT (..))
import           Database.Persist                (Entity(..), (==.))
import qualified Database.Persist.Class         as Db
import           Gonimo.Database.Effects.Servant
import           Gonimo.Server.Auth
import           Gonimo.Server.Handlers.Auth
import           Gonimo.Server.Handlers.Session
import           Gonimo.Server.Handlers.Socket
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects           hiding (Server, ServerT)
import qualified Gonimo.Server.Effects           as Gonimo
import           Gonimo.Server.Error    (throwServant, ServerError (..))
import           Gonimo.Server.Handlers
import           Gonimo.Server.Types
import           Gonimo.WebAPI
import           Servant                ((:<|>) (..), (:~>) (..),
                                         ServantErr (..), Server, ServerT,
                                         enter)
import           Servant.Server         (err401)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import           Control.Exception.Lifted        (try)
import           Control.Monad.Logger           (LoggingT)
import           Control.Monad.IO.Class         (MonadIO)

-- TODO: This does not really belong here:
type AuthServer = ReaderT AuthData (Gonimo.ServerT (LoggingT IO))

effServer :: ServerT GonimoAPI Gonimo.Server
effServer =  createDevice
        :<|> getAuthServer
        :<|> getCoffee

authServer :: ServerT AuthGonimoAPI AuthServer
authServer = invitationsServer
        :<|> accountsServer
        :<|> familiesServer
        :<|> socketServer
        :<|> sessionServer

invitationsServer :: ServerT InvitationsAPI AuthServer
invitationsServer = createInvitation
               :<|> answerInvitation
               :<|> sendInvitation
               :<|> putInvitationInfo

accountsServer :: ServerT AccountsAPI AuthServer
accountsServer = getAccountFamilies

familiesServer :: ServerT FamiliesAPI AuthServer
familiesServer = createFamily
            :<|> familyServer

familyServer :: FamilyId -> ServerT FamilyAPI AuthServer
familyServer familyId = getFamily familyId
                   :<|> getDeviceInfos familyId

socketServer :: ServerT SocketAPI AuthServer
socketServer = createChannel
          :<|> receiveChannel
          :<|> deleteChannelRequest
          :<|> putMessage
          :<|> receiveMessage
          :<|> deleteMessage

sessionServer :: ServerT SessionAPI AuthServer
sessionServer = sessionRegisterR
          :<|> sessionUpdateR
          :<|> sessionDeleteR
          :<|> sessionListDevicesR


-- Let's serve
getServer :: (forall m a. MonadIO m => LoggingT m a -> m a) -> Config -> Server GonimoAPI
getServer runLoggingT c = enter (gonimoToServant runLoggingT c) effServer

getAuthServer :: Maybe AuthToken -> ServerT AuthGonimoAPI Gonimo.Server
getAuthServer s = enter (authToEff s) authServer


----------------------- Natural transformations -------------------------

authToEff' :: Maybe AuthToken -> AuthServer a -> Gonimo.Server a
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
    runReaderT m authData
  where
    getByAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.getBy
    getAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.get

authToEff :: Maybe AuthToken -> AuthServer :~> Gonimo.Server
authToEff token = Nat $ authToEff' token

-------

gonimoToServant' :: (forall m. MonadIO m => LoggingT m a -> m a)
              -> Config -> Gonimo.Server a -> ExceptT ServantErr IO a
gonimoToServant' runLoggerT c = handleExceptions . runServer runLoggerT c

gonimoToServant :: (forall m a. MonadIO m => LoggingT m a -> m a)
             -> Config -> Gonimo.Server :~> ExceptT ServantErr IO
gonimoToServant runLoggerT c = Nat $ gonimoToServant' runLoggerT c


-------------------------------------------------------------------------


---------------------- Exception handling -------------------------------

handleExceptions :: IO a -> ExceptT ServantErr IO a
handleExceptions = ExceptT . try
