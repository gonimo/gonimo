{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Gonimo.SocketServer where

import           Control.Exception.Lifted       (throwIO)
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger           (LoggingT)
import           Control.Monad.Logger           (logError)
import           Control.Monad.Reader.Class     (MonadReader)
import           Control.Monad.State.Class      (MonadState, get, put)
import           Control.Monad.Trans.Reader     (ReaderT, runReaderT)
import           Control.Monad.Trans.State      (StateT, evalStateT)
import           Data.Aeson                     (FromJSON)
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy           as LB
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Database.Persist               (Entity (..), (==.))
import qualified Database.Persist.Class         as Db
import           Gonimo.Db.Entities
import           Gonimo.Server.Auth             (AuthData(..), AuthReader)
import           Gonimo.Server.Effects
import           Gonimo.Server.Error            (ServerError (..), fromMaybeErr)
import qualified Gonimo.Server.Error            as Error
import           Gonimo.Server.Handlers
import           Gonimo.Server.Handlers.Auth
import           Gonimo.SocketAPI
import           Gonimo.Types                   (AuthToken(..))
import qualified Network.HTTP.Types.Status      as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WS
import           Gonimo.Database.Effects.Servant (serverErrOnNothing)


handleServerRequest :: (MonadState (Maybe AuthData) m, MonadServer m) => ServerRequest -> m ServerResponse
handleServerRequest reqBody = case reqBody of
  ReqMakeDevice userAgent -> ResMadeDevice <$> createDevice userAgent
  ReqAuthenticate token    -> authenticate token
  _                       -> do
    authData' <- fromMaybeErr NotAuthenticated =<< get
    flip runReaderT authData' $ handleAuthServerRequest reqBody

handleAuthServerRequest :: (AuthReader m, MonadServer m) => ServerRequest -> m ServerResponse
handleAuthServerRequest req = case req of
  ReqMakeDevice _    -> error "ReqMakeDevice should have been handled already!"
  ReqAuthenticate _  -> error "ReqAuthenticate should have been handled already!"
  ReqCreateFamily    -> ResCreatedFamily <$> createFamily
  ReqCreateInvitation familyId' -> ResCreatedInvitation <$> createInvitation familyId'

serveWebSocket :: forall m. (MonadState (Maybe AuthData) m, MonadServer m) => WS.Connection -> m ()
serveWebSocket conn = forever handleIncoming
  where
    handleIncoming :: m ()
    handleIncoming = do
      raw <- liftIO $ WS.receiveDataMessage conn
      let bs = case raw of
                WS.Binary bs' -> bs'
                WS.Text bs' -> bs'
      decoded <- decodeLogError bs
      r <- handleServerRequest decoded
      let encoded = Aeson.encode r
      liftIO . WS.sendDataMessage conn $ WS.Text encoded

    decodeLogError :: forall a. FromJSON a => LB.ByteString -> m a
    decodeLogError bs = do
      let r = Aeson.eitherDecode bs
      case r of
        Left err -> do
          $logError ("Request could not be decoded: " <> T.pack err)
          throwIO $ userError "Request could not be decoded!"
        Right ok -> pure ok

serve :: (forall a m. MonadIO m => LoggingT m a -> m a) -> Config -> Wai.Application
serve runLoggingT config = do
  let handleWSConnection pending = do
        connection <- WS.acceptRequest pending
        WS.forkPingThread connection 28
        runServer runLoggingT config . flip evalStateT Nothing $ serveWebSocket connection
  let
    errorApp :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    errorApp _ sendResponse =  do
      let response = Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
      sendResponse response

  Wai.websocketsOr WS.defaultConnectionOptions handleWSConnection errorApp

authenticate :: forall m. (MonadState (Maybe AuthData) m, MonadServer m)
  => AuthToken -> m ServerResponse
authenticate token = do
  authData' <- makeAuthData token
  put $ Just authData'
  pure ResAuthenticated

makeAuthData :: MonadServer m => AuthToken -> m AuthData
makeAuthData token = runDb $ do
    device@(Entity _ Device{..}) <- getByAuthErr $ AuthTokenDevice token
    account <- getAuthErr deviceAccountId
    fids <- map (familyAccountFamilyId . entityVal)
            <$> Db.selectList [FamilyAccountAccountId ==. deviceAccountId] []
    return $ AuthData (Entity deviceAccountId account) fids device
  where
    getByAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.getBy
    getAuthErr = serverErrOnNothing InvalidAuthToken <=< Db.get
