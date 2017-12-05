{-|
Module      : Gonimo.Server.Session.Internal
Description : Types and internal functions for "Gonimo.Server.Session"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Session.Internal where


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

newtype Session
  = Session { -- | Call this function for sending a message to the client.
              --
              --   It will throw, if the client is no longer available.
              _sendMessage :: ToClient -> IO ()
            }

data Impl
  = Impl { __session  :: Session
           -- | When authenticated this variable holds the device id of the connected device.
         , _sessionId :: !(IORef (Maybe Deviceid))
         }

make :: Config -> WS.Connection -> IO ()
make = undefined
