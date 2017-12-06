{-|
Module      : Gonimo.Server.Session.Internal
Description : Types and internal functions for "Gonimo.Server.Session"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Session.Internal where

import           Data.Aeson                       (FromJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString.Lazy             as LB
import           Control.Logging.Extended
import           Data.Text (Text)
import qualified Data.Text as T


import Gonimo.Server.Error
import Gonimo.SocketAPI
import Gonimo.Constants
import Gonimo.Prelude

-- | "Server.Config" for database access and callbacks for transmitting client events.
data Config
  = Config {
              _serverConfig :: Server.Config
              -- | Gets called on every message that comes from the device,
              --   except for the low lovel session commands
              --   'Authenticate', 'Ping' and 'MakeDevice'.
            , _receiveMessage :: (DeviceId, FromClient) -> IO ()

              -- | Will get called once the device has been authenticated. (It
              --   has the secret to it's device id.)
            , _authenticated  :: (DeviceId, Session) -> IO ()

              -- | The device is gone and no longer available.
              --
              --   It has quit the session.
            , _quit           :: DeviceId -> IO ()

            }

-- | Once the client is authenticated '_authenticated' will be called with 'Session' data.
--
--   It can be used for communicating with the client.
newtype Session
  = Session { -- | Call this function for sending a message to the client.
              --
              --   It will throw, if the client is no longer available.
              _sendMessage :: ToClient -> IO ()
            }

-- | Internal data for implementing client sessions.
data Impl
  = Impl { __session  :: Session
           -- | When authenticated this variable holds the device id of the connected device.
         , _sessionId :: !(IORef (Maybe Deviceid))

         , _msgCounter :: !(IORef Int)
         , _connection :: WS.Connection
         }

-- | Tag for log messages from this module.
logSource :: Text
logSource = "Session"


-- | Decode and receive messages.
receiveMessages :: Config -> Impl -> IO ()
receiveMessages conf impl = forever $ do
    raw <- liftIO . WS.receiveDataMessage (impl^.connection)
    modifyIORef' msgCounter (+1)
    let bs = case raw of
              WS.Binary bs' -> bs'
              WS.Text bs' -> bs'
    handleMessage impl conf =<< decodeLogError bs

noUserAgentDefault :: Text
noUserAgentDefault = "None"

maxUserAgentLength :: Int
maxUserAgentLength = 300

-- | Handle a single message from the client.
handleMessage :: Config -> Impl -> FromClient -> IO ()
handleMessage impl msg
  = reportExceptions $ case msg of
      Ping -> impl^._session.sendMessage $ Pong
      MakeDevice userAgent -> makeDevice impl conf userAgent
      Authenticate token -> authenticate token
      _ -> do
        mDevId <- readIORef $ impl^.sessionId
        devId <- fromMaybeErr NotAuthenticated mDevId
        conf^.receiveMessage $ (devId, msg)
  where
    reportExceptions :: IO () -> IO ()
    reportExceptions process = process `catch` reportError

    reportError :: ServerError -> IO ()
    reportError e = do
      errorS logSource $ "Caught server error:\n" <> (T.pack . show) e
      impl^._session.sendMessage $ ServerError msg e

-- | Create an anonymous account and a device.
--   Each device is uniquely identified by a DeviceId, multiple
--   Device's can share a single account in case a user login was provided,
--   otherwise every device corresponds to a single Account.
createDeviceR :: Config -> Impl -> Maybe Text -> IO ()
createDeviceR conf impl mUserAgent = do
  now <- getCurrentTime
  authToken' <- GonimoSecret <$> generateSecret
  let userAgent = maybe noUserAgentDefault (T.take maxUserAgentLength) mUserAgent
  runDb conf $ do
    aid <- Account.insert
      $ Account { accountCreated = now
                }

    void $ Device.insert
      $ Device  { deviceName         = Nothing -- Will be set on join of first family
                , deviceAuthToken    = authToken'
                , deviceAccountId    = aid
                , deviceLastAccessed = now
                , deviceUserAgent    = userAgent
                }
  impl^._session.sendMessage $ MadeDevice authToken'

-- | Find secret in db, initialize sessionId and call conf^.authenticated.
authenticate :: forall m.  AuthToken -> IO ()
authenticate impl conf token = do
  (deviceId', _) <- runDb conf $ Device.getByAuthToken token
  writeIORef (impl^.sessionId) $ Just deviceId'
  conf^.authenticated $ (deviceId', impl^._session)


-- Kill session after 'serverWatchDogTime'
watchDog :: IORef Int -> IO ()
watchDog msgCount = forever $ do
  currentCount <- readIORef msgCount
  threadDelay $ (ceiling serverWatchDogTime) * 1000000
  nextCount <- readIORef msgCount
  when (nextCount == currentCount) $ throwIO $ WS.CloseRequest 1000 "Connection timed out (server)"


-- | Decode a ByteString, logging any error to console and throwing.
decodeLogError :: forall a. FromJSON a => LB.ByteString -> IO a
decodeLogError bs = do
  let r = Aeson.eitherDecode bs
  case r of
    Left err -> do
      errorS logSource $ "Request could not be decoded: " <> T.pack err
      throwIO $ userError "Request could not be decoded!"
    Right ok -> pure ok


instance Server.HasConfig Config where
  config = serverConfig

-- Lenses:

class HasConfig a where
  config :: Lens' a Config

  serverConfig :: Lens' a Server.Config
  serverConfig = config . go
    where
      go :: Lens' Config Server.Config
      go f config' = (\serverConfig' -> config' { _serverConfig = serverConfig' }) <$> f (_serverConfig config')


  receiveMessage :: Lens' a ((DeviceId, FromClient) -> IO ())
  receiveMessage = config . go
    where
      go :: Lens' Config ((DeviceId, FromClient) -> IO ())
      go f config' = (\receiveMessage' -> config' { _receiveMessage = receiveMessage' }) <$> f (_receiveMessage config')


  authenticated :: Lens' a ((DeviceId, Session) -> IO ())
  authenticated = config . go
    where
      go :: Lens' Config ((DeviceId, Session) -> IO ())
      go f config' = (\authenticated' -> config' { _authenticated = authenticated' }) <$> f (_authenticated config')


  quit :: Lens' a (DeviceId -> IO ())
  quit = config . go
    where
      go :: Lens' Config (DeviceId -> IO ())
      go f config' = (\quit' -> config' { _quit = quit' }) <$> f (_quit config')


instance HasConfig Config where
  config = id

class HasImpl a where
  impl :: Lens' a Impl

  _session :: Lens' a Session
  _session = impl . go
    where
      go :: Lens' Impl Session
      go f impl' = (\_session' -> impl' { __session = _session' }) <$> f (__session impl')


  sessionId :: Lens' a ((IORef (Maybe Deviceid)))
  sessionId = impl . go
    where
      go :: Lens' Impl ((IORef (Maybe Deviceid)))
      go f impl' = (\sessionId' -> impl' { _sessionId = sessionId' }) <$> f (_sessionId impl')


  msgCounter :: Lens' a ((IORef Int))
  msgCounter = impl . go
    where
      go :: Lens' Impl ((IORef Int))
      go f impl' = (\msgCounter' -> impl' { _msgCounter = msgCounter' }) <$> f (_msgCounter impl')


  connection :: Lens' a WS.Connection
  connection = impl . go
    where
      go :: Lens' Impl WS.Connection
      go f impl' = (\connection' -> impl' { _connection = connection' }) <$> f (_connection impl')


instance HasImpl Impl where
  impl = id

