{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Gonimo.SocketServer where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch              as X (MonadThrow (..))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Logger             (logError)
import           Control.Monad.Reader.Class       (ask)
import           Control.Monad.Trans.Maybe        (MaybeT (..), runMaybeT)
import           Data.Aeson                       (FromJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString.Lazy             as LB
import           Data.IORef
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Network.HTTP.Types.Status        as Http
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Handler.WebSockets   as Wai
import qualified Network.WebSockets               as WS
import           UnliftIO.Async                   (async, race_)
-- Does not yet exist in version 0.1.0.0:
-- import           UnliftIO.Concurrent              (forkIO)
import           UnliftIO.Exception               (catch, finally, throwIO, try)

import           Gonimo.Constants
import           Gonimo.Server.Auth               (AuthData (..),
                                                   HasAuthData (..),
                                                   allowedFamilies)

import qualified Gonimo.Server.Db.Account         as Account
import qualified Gonimo.Server.Db.Device          as Device
import           Gonimo.Server.Config            as Server
import           Gonimo.Server.Error              (ServerError (..))
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

-- | AuthData and Server 'Config' combined.
--
--   Has 'HasAuthData' and 'HasConfig' instances. Accessors are not expected to
--   be used except for initialization.
data AuthConfig = AuthConfig { __authData :: AuthData
                             , __config   :: Config
                             }

$(makeLenses 'AuthConfig)

-- | 'AuthConfig' has 'AuthData'.
instance HasAuthData AuthConfig where
  authData = _authData

-- | 'AuthConfig' has 'Config'.
instance HasConfig AuthConfig where
  config = _config

serve :: Config -> Wai.Application
serve config' = do
  let handleWSConnection pending = do
        connection <- WS.acceptRequest pending
        -- Don't use this - the client won't be able to detect dead connections, which is baaad! (Baby station!)
        -- WS.forkPingThread connection 28
        noAuthRef <- newIORef Nothing
        runRIO config' $ serveWebSocket noAuthRef connection
  let
    errorApp :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    errorApp _ sendResponse =  do
      let response = Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
      sendResponse response

  Wai.websocketsOr WS.defaultConnectionOptions handleWSConnection errorApp


serveWebSocket :: AuthDataRef -> WS.Connection -> ServerIO ()
serveWebSocket authRef conn = do
    msgCounter <- liftIO $ newIORef 0
    sub <- view configSubscriber
    client <- atomically $ Subscriber.makeClient sub
    let
      work = race_ (liftIO $ watchDog msgCounter)
                   (race_ (Subscriber.runMonitor client respondToRequest) (forever handleIncoming))

      cleanup = do
        Subscriber.cleanup client
        runAuthServer (pure ()) authRef (deleteReceiverR =<< view authDeviceId)

      handleIncoming = do
        raw <- liftIO $ WS.receiveDataMessage conn
        liftIO $ modifyIORef' msgCounter (+1)
        let bs = case raw of
                  WS.Binary bs' -> bs'
                  WS.Text bs'   -> bs'
        respondToRequest =<< decodeLogError bs


      respondToRequest :: ServerRequest -> ServerIO ()
      respondToRequest req = void . async $ do -- Let's fork off :-)
        r <- handleServerRequest authRef receiver client req -- Does handle user interesting exceptions ...
        case r of
          ResGotFamilies _ fids -> liftIO $ modifyIORef' authRef (_Just.allowedFamilies .~ fids)
          _ -> pure ()
        sendWSMessage r

      sendWSMessage r = do
        let encoded = Aeson.encode r
        liftIO . WS.sendDataMessage conn $ WS.Text encoded

      receiver :: Message -> IO ()
      receiver message = wsExceptionToServerError $ case message of
          MessageSessionGotStolen
            -> do
            writeIORef authRef Nothing
            sendWSMessage EventSessionGotStolen
          MessageCreateChannel fromId secret
            -> sendWSMessage $ EventChannelRequested fromId secret
          MessageSendMessage fromId secret msg
            -> sendWSMessage $ EventMessageReceived fromId secret msg
        where
          wsExceptionToServerError :: IO () -> IO ()
          wsExceptionToServerError action = catch action
            $ \e -> case e of
                      WS.CloseRequest _ _ -> throwIO DeviceOffline
                      WS.ConnectionClosed -> throwIO DeviceOffline
                      _                   -> throwIO InternalServerError

      decodeLogError :: forall a. FromJSON a => LB.ByteString -> ServerIO a
      decodeLogError bs = do
        let r = Aeson.eitherDecode bs
        case r of
          Left err -> do
            $logError ("Request could not be decoded: " <> T.pack err)
            throwM $ userError "Request could not be decoded!"
          Right ok -> pure ok

    finally work cleanup



handleServerRequest :: AuthDataRef -> (Message -> IO ()) -> Subscriber.Client -> ServerRequest -> ServerIO ServerResponse
handleServerRequest authRef receiver sub req = errorToResponse $ case req of
    ReqPing                 -> pure ResPong
    ReqAuthenticate token   -> authenticate authRef receiver sub token
    ReqMakeDevice userAgent -> ResMadeDevice <$> createDeviceR userAgent
    _                       -> runAuthServer (throwM NotAuthenticated) authRef
                               $ handleAuthServerRequest sub req
  where
    errorToResponse :: ServerIO ServerResponse -> ServerIO ServerResponse
    errorToResponse action = do
      r <- either (ResError req) id <$> try action
      case r of
        ResError _ _ -> $logError ((T.pack. show) r)
        _            -> pure ()
      pure r


handleAuthServerRequest :: (HasAuthData env, HasConfig env) => Subscriber.Client -> ServerRequest -> RIO env ServerResponse
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
  ReqSetFamilyName familyId name       -> setFamilyNameR familyId name
                                          *> pure (ResSetFamilyName familyId)
  ReqGetFamily familyId                -> ResGotFamily familyId <$> getFamilyR familyId
  ReqGetFamilyMembers familyId         -> ResGotFamilyMembers familyId <$> getFamilyMembersR familyId
  ReqGetOnlineDevices familyId         -> ResGotOnlineDevices familyId <$> getOnlineDevicesR familyId
  ReqSaveBabyName familyId name        -> saveBabyNameR familyId name
                                          *> pure ResSavedBabyName

  ReqCreateInvitation familyId'        -> ResCreatedInvitation <$> createInvitationR familyId'
  ReqSendInvitation sendInv            -> do sendInvitationR sendInv
                                             pure $ ResSentInvitation sendInv
  ReqClaimInvitation secret            -> ResClaimedInvitation secret <$> claimInvitationR secret
  ReqClaimInvitationByCode invCode     -> uncurry (ResClaimedInvitationByCode invCode) <$> claimInvitationByCodeR invCode
  ReqAnswerInvitation secret reply     -> ResAnsweredInvitation secret reply <$> answerInvitationR secret reply
  ReqGetFamilyInvitations familyId     -> ResGotFamilyInvitations familyId <$> getFamilyInvitationsR familyId
  ReqGetInvitation invId               -> ResGotInvitation invId <$> getInvitationR invId
  ReqCreateInvitationCode invId        -> uncurry (ResCreatedInvitationCode invId) <$> createInvitationCodeR invId

  ReqSetSubscriptions subs             -> do
    accountId <- view authAccountId
    -- We need GetFamilies subscribed to keep our AuthData current.
    let subsSet = Set.insert (ReqGetFamilies accountId) (Set.fromList subs)
    Server.atomically $ Subscriber.processRequest sub subsSet
    pure ResSubscribed

  ReqGetFamilies accountId             -> ResGotFamilies accountId <$> getFamiliesR accountId
  ReqGetDevices accountId              -> ResGotDevices  accountId <$> getDevicesR accountId
  ReqLeaveFamily accountId familyId    -> do leaveFamilyR accountId familyId
                                             pure $ ResLeftFamily accountId familyId

  ReqCreateChannel from' to'             -> ResCreatedChannel from' to' <$> createChannelR from' to'
  ReqSendMessage from' to' secret msg    -> sendMessageR from' to' secret msg *> pure ResSentMessage

authenticate :: AuthDataRef -> (Message -> IO ()) -> Subscriber.Client -> AuthToken -> ServerIO ServerResponse
authenticate authRef receiver sub token = do
  authData' <- makeAuthData token
  liftIO . writeIORef authRef $ Just authData'
  -- Evil hack:
  atomically . Subscriber.processRequest sub $ Set.singleton (ReqGetFamilies (authData' ^. authAccountId))
  runAuthServer (pure ()) authRef $ registerReceiverR (authData' ^. authDeviceId) receiver
  pure ResAuthenticated

makeAuthData :: HasConfig env => AuthToken -> RIO env AuthData
makeAuthData token = runDb $ do
    (devId, dev@Device{..}) <- Device.getByAuthToken token
    account <- Account.get deviceAccountId
    fids <- Account.getFamilyIds deviceAccountId
    return $ AuthData { _authAccountId = deviceAccountId
                      , _authAccount = account
                      , _allowedFamilies = fids
                      , _authDeviceId = devId
                      , _authDevice = dev
                      }

-- Clean up dead connections after a sensible delay:
watchDog :: IORef Int -> IO ()
watchDog msgCount = forever $ do
  currentCount <- readIORef msgCount
  threadDelay $ (ceiling serverWatchDogTime) * 1000000
  nextCount <- readIORef msgCount
  when (nextCount == currentCount) $ throwIO $ WS.CloseRequest 1000 "Connection timed out (server)"

-- | Convenience function for running a handler that requires AuthData.
runAuthServer ::
  ServerIO a -- ^ Fallback action in case authRef was 'Nothing'
  -> AuthDataRef -- ^ The 'IORef' to receive the needed 'AuthData' from.
  -> RIO AuthConfig a -- ^ The 'AuthData' needing action to run.
  -> ServerIO a
runAuthServer fallback authRef authAction = maybe fallback return <=< runMaybeT $ do
    authData' <- MaybeT . liftIO $ readIORef authRef
    conf <- ask
    let authConf = AuthConfig { __authData = authData', __config = conf }
    runRIO authConf $ authAction
