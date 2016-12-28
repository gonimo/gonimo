{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Gonimo.SocketServer where

import           Control.Monad                   (forever)
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class (liftIO)
import           Gonimo.Server.Effects
import           Gonimo.Server.Handlers
import           Gonimo.SocketAPI
import           Control.Exception.Lifted        (throwIO)
import           Control.Monad.Logger           (LoggingT)
import           Control.Monad.IO.Class         (MonadIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Network.WebSockets.Connection   as WS
import qualified Network.WebSockets   as WS
import qualified Data.Aeson as Aeson
import  Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as LB
import Control.Monad.Logger (logError)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Data.Text as T


handleAPIRequest :: MonadServer m => APIRequest -> m APIResponse
handleAPIRequest req = case req of
  APIRequest mId reqBody -> APIResponse mId <$> handleRequestBody reqBody

handleRequestBody :: MonadServer m => RequestBody -> m ResponseBody
handleRequestBody reqBody = case reqBody of
  ReqMakeDevice userAgent -> ResMakeDevice <$> createDevice userAgent


serveWebSocket :: forall m. (MonadServer m) => WS.Connection -> m ()
serveWebSocket conn = forever handleIncoming
  where
    handleIncoming :: m ()
    handleIncoming = do
      raw <- liftIO $ WS.receiveDataMessage conn
      case raw of
        WS.Binary _ -> error "Sorry - binary connections are currently not supported!"
        WS.Text bs -> do
          decoded <- decodeLogError bs
          r <- handleAPIRequest decoded
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
        runServer runLoggingT config $ serveWebSocket connection
  let
    errorApp :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    errorApp _ sendResponse =  do
      let response = Wai.responseLBS (Http.Status 400 "WebSocket Only Server") [] ""
      sendResponse response

  Wai.websocketsOr WS.defaultConnectionOptions handleWSConnection errorApp
