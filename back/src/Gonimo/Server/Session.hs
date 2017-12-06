{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Server.Session
Description : A running client session, it handles client messages and forwards server events to the client.
Copyright   : (c) Robert Klotzner, 2017
This is the backend for the "Gonimo.Server.Sessions" reflex module, which does the low-level plumbing for lifting everything to events and behaviors in "Gonimo.Server.Sessions".
-}
module Gonimo.Server.Session ( -- * Types and classes
                               Config (..)
                             , HasConfig (..)
                             , Session (..)
                             , HasSession (..)
                             , make
                             ) where


import           Control.Logging.Extended
import qualified Data.Aeson                     as Aeson
import           Data.IORef
import qualified Network.WebSockets             as WS
import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Lens

import           Gonimo.Prelude
import qualified Gonimo.Server.Config           as Server
import           Gonimo.Server.Session.Internal
import           Gonimo.SocketAPI.APIVersion





-- | Make a new session for a given pending WS connection.
make :: (HasConfig c, Server.HasConfig c)
  => c -- ^ The configuration to use. Db connections & callbacks to call on client events.
  -> APIVersion -- ^ The API version the connected client expects. (Currently unused, as we only have one version.)
  -> WS.PendingConnection -- ^ The WebSocket connection to accept and handle requests from.
  -> IO ()
make conf _ pending = do
    _msgCounter <- newIORef 0
    _sessionId <- newIORef Nothing
    _connection <- WS.acceptRequest pending
    let
      sendWSMessage msg = do
        let encoded = Aeson.encode msg
        deviceId' <- readIORef _sessionId -- On 'Nothing' just log "Nothing"
        safeSend deviceId' encoded

      safeSend deviceId'
        = void . logExceptionS logSource ("Sending message to device failed: " <> (T.pack . show) deviceId')
          . WS.sendDataMessage _connection . WS.Text

      __session = Session sendWSMessage

    let impl = Impl {..}

    let
      work = race_ (watchDog _msgCounter) (receiveMessages conf impl)

      cleanup = do
        mId <- readIORef _sessionId
        traverse_ (conf^.quit) mId -- On Nothing -> nothing to do!

    finally work cleanup


