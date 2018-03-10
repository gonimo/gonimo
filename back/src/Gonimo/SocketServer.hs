{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Gonimo.SocketServer where

import           Control.Concurrent
import           Control.Concurrent.Async.Lifted  (race_)
import           Control.Exception.Lifted         (catch, finally, throwIO, try)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class           (liftIO, MonadIO)
import           Control.Monad.Logger             (LoggingT, logError)
import           Control.Monad.Reader.Class       (MonadReader, ask)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT (..), runMaybeT)
import           Control.Monad.Trans.Reader       (runReaderT)
import           Data.Aeson                       (FromJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString.Lazy             as LB
import           Data.IORef
import           Data.Maybe
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Network.HTTP.Types.Status        as Http
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Handler.WebSockets   as Wai
import qualified Network.WebSockets               as WS

import           Gonimo.Constants
import           Gonimo.Server.Auth               (AuthData (..), AuthReader,
                                                   allowedFamilies)
import qualified Gonimo.Server.Auth               as Auth
import qualified Gonimo.Server.Db.Account         as Account
import qualified Gonimo.Server.Db.Device          as Device
import           Gonimo.Server.Effects            as Server
import           Gonimo.Server.Error              (ServerError (..),
                                                   fromMaybeErr)
import           Gonimo.Server.Handlers
import           Gonimo.Server.Handlers.Auth
import           Gonimo.Server.Handlers.Messenger
import           Gonimo.Server.Handlers.Socket
import           Gonimo.Server.Messenger          (Message (..))
import qualified Gonimo.Server.Subscriber         as Subscriber
import qualified Gonimo.Server.Subscriber.Types   as Subscriber
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types           hiding (AuthData (..),
                                                          Message (..))
import           Gonimo.Types                     (AuthToken (..))

type AuthDataRef = IORef (Maybe AuthData)

serve :: (forall a m. MonadIO m => LoggingT m a -> m a) -> Config -> Wai.Application
serve runLoggingT config = do
  let handleWSConnection pending = do
        connection <- WS.acceptRequest pending
        -- Don't use this - the client won't be able to detect dead connections, which is baaad! (Baby station!)
        -- WS.forkPingThread connection 28
        noAuthRef <- newIORef Nothing
        runServer runLoggingT config $ serveWebSocket noAuthRef connection
  let
    errorApp :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    errorApp _ sendResponse =  do
      let response = Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
      sendResponse response

  Wai.websocketsOr WS.defaultConnectionOptions handleWSConnection errorApp


serveWebSocket :: AuthDataRef -> WS.Connection -> Server ()
serveWebSocket authRef conn = do
    msgCounter <- liftIO $ newIORef 0
    sub <- configSubscriber <$> ask
    client <- atomically $ Subscriber.makeClient sub
    let
      work = race_ (liftIO $ watchDog msgCounter)
                   (race_ (Subscriber.runMonitor client respondToRequest) (forever handleIncoming))
      cleanup = do
        Subscriber.cleanup client
        fmap (fromMaybe ()) . runMaybeT $ do
          authData' <- MaybeT . liftIO $ readIORef authRef
          lift . flip runReaderT authData' $ deleteReceiverR (Auth.deviceKey authData')

      handleIncoming = do
        raw <- liftIO $ WS.receiveDataMessage conn
        liftIO $ modifyIORef' msgCounter (+1)
        let bs = case raw of
                  WS.Binary bs' -> bs'
                  WS.Text bs' -> bs'
        respondToRequest =<< decodeLogError bs

      respondToRequest :: ServerRequest -> Server ()
      respondToRequest req = async_ $ do -- Let's fork off :-)
        r <- flip runReaderT authRef
             $ handleServerRequest receiver client req -- Does handle user interesting exceptions ...
        case r of
          ResGotFamilies _ fids -> liftIO $ modifyIORef' authRef (_Just.allowedFamilies .~ fids)
          _ -> pure ()
        sendWSMessage r

      sendWSMessage r = do
        let encoded = Aeson.encode r
        liftIO . WS.sendDataMessage conn $ WS.Text encoded

      receiver :: Message -> IO ()
      receiver message = wsExceptionToServerError $
        case message of
          MessageSessionGotStolen
            -> do writeIORef authRef Nothing
                  sendWSMessage EventSessionGotStolen
          MessageCreateChannel fromId secret
            -> sendWSMessage $ EventChannelRequested fromId secret
          MessageSendMessage fromId secret msg
            -> sendWSMessage $ EventMessageReceived fromId secret msg
        where
          wsExceptionToServerError :: IO () -> IO ()
          wsExceptionToServerError action = catch action
            $ \case WS.CloseRequest _ _ -> throwIO DeviceOffline
                    WS.ConnectionClosed -> throwIO DeviceOffline
                    _ -> throwIO InternalServerError

      decodeLogError :: forall a. FromJSON a => LB.ByteString -> Server a
      decodeLogError bs =
        case Aeson.eitherDecode bs of
          Left err -> do
            $logError ("Request could not be decoded: " <> T.pack err)
            throwIO $ userError "Request could not be decoded!"
          Right ok -> pure ok

    finally work cleanup



handleServerRequest :: forall m. (MonadReader AuthDataRef m, MonadServer m)
                       => (Message -> IO ()) -> Subscriber.Client -> ServerRequest -> m ServerResponse
handleServerRequest receiver sub req = errorToResponse $ case req of
    ReqPing                 -> pure ResPong
    ReqAuthenticate token   -> authenticate receiver sub token
    ReqMakeDevice userAgent -> ResMadeDevice <$> createDeviceR userAgent
    _                       -> do
      authData' <- fromMaybeErr NotAuthenticated =<< liftIO . readIORef =<< ask
      flip runReaderT authData' $ handleAuthServerRequest sub req
  where
    errorToResponse :: m ServerResponse -> m ServerResponse
    errorToResponse action = do
      r <- either (ResError req) id <$> try action
      case r of
        ResError _ _ -> $logError ((T.pack. show) r)
        _ -> pure ()
      pure r


handleAuthServerRequest :: (AuthReader m, MonadServer m) => Subscriber.Client -> ServerRequest -> m ServerResponse
handleAuthServerRequest sub req = case req of
  ReqPing                              -> error "ReqPing should have been handled already!"
  ReqAuthenticate _                    -> error "ReqAuthenticate should have been handled already!"
  ReqMakeDevice _                      -> error "ReqMakeDevice should have been handled already!"
  ReqGetDeviceInfo deviceId            -> ResGotDeviceInfo deviceId <$> getDeviceInfoR deviceId
  ReqSetDeviceType deviceId deviceType -> do setDeviceTypeR deviceId deviceType
                                             pure $ ResSetDeviceType deviceId
  ReqSetDeviceName deviceId deviceName -> do setDeviceNameR deviceId deviceName
                                             pure $ ResSetDeviceName deviceId
  ReqSwitchFamily deviceId familyId    -> do switchFamilyR deviceId familyId
                                             pure $ ResSwitchedFamily deviceId familyId
  ReqCreateFamily                      -> ResCreatedFamily <$> createFamilyR
  ReqSetFamilyName familyId name       -> ResSetFamilyName familyId <$ setFamilyNameR familyId name
  ReqGetFamily familyId                -> ResGotFamily familyId <$> getFamilyR familyId
  ReqGetFamilyMembers familyId         -> ResGotFamilyMembers familyId <$> getFamilyMembersR familyId
  ReqGetOnlineDevices familyId         -> ResGotOnlineDevices familyId <$> getOnlineDevicesR familyId
  ReqSaveBabyName familyId name        -> ResSavedBabyName <$ saveBabyNameR familyId name
  ReqCreateInvitation familyId'        -> ResCreatedInvitation <$> createInvitationR familyId'
  ReqSendInvitation sendInv            -> do sendInvitationR sendInv
                                             pure $ ResSentInvitation sendInv
  ReqClaimInvitation secret            -> ResClaimedInvitation secret <$> claimInvitationR secret
  ReqAnswerInvitation secret reply     -> ResAnsweredInvitation secret reply <$> answerInvitationR secret reply

  ReqSetSubscriptions subs             -> do
    accountId <- Auth.accountKey <$> ask
    -- We need GetFamilies subscribed to keep our AuthData current.
    let subsSet = Set.insert (ReqGetFamilies accountId) (Set.fromList subs)
    Server.atomically $ Subscriber.processRequest sub subsSet
    pure ResSubscribed

  ReqGetFamilies accountId             -> ResGotFamilies accountId <$> getFamiliesR accountId
  ReqGetDevices accountId              -> ResGotDevices  accountId <$> getDevicesR accountId
  ReqLeaveFamily accountId familyId    -> do leaveFamilyR accountId familyId
                                             pure $ ResLeftFamily accountId familyId

  ReqCreateChannel from' to'           -> ResCreatedChannel from' to' <$> createChannelR from' to'
  ReqSendMessage from' to' secret msg  -> ResSentMessage <$ sendMessageR from' to' secret msg

authenticate :: forall m. (MonadReader AuthDataRef m, MonadServer m)
  => (Message -> IO ()) -> Subscriber.Client -> AuthToken -> m ServerResponse
authenticate receiver sub token = do
  authData' <- makeAuthData token
  authRef <- ask
  liftIO . writeIORef authRef $ Just authData'
  -- Evil hack:
  atomically . Subscriber.processRequest sub $ Set.singleton (ReqGetFamilies (Auth.accountKey authData'))
  flip runReaderT authData' $ registerReceiverR (Auth.deviceKey authData') receiver
  pure ResAuthenticated

makeAuthData :: MonadServer m => AuthToken -> m AuthData
makeAuthData token = runDb $ do
    (devId, dev@Device{..}) <- Device.getByAuthToken token
    account <- Account.get deviceAccountId
    fids <- Account.getFamilyIds deviceAccountId
    pure AuthData { _authAccountId   = deviceAccountId
                  , _authAccount     = account
                  , _allowedFamilies = fids
                  , _authDeviceId    = devId
                  , _authDevice      = dev
                  }

-- Clean up dead connections after a sensible delay:
watchDog :: IORef Int -> IO ()
watchDog msgCount = forever $ do
  currentCount <- readIORef msgCount
  threadDelay $ (ceiling serverWatchDogTime) * 1000000
  nextCount <- readIORef msgCount
  when (nextCount == currentCount) $ throwIO $ WS.CloseRequest 1000 "Connection timed out (server)"
