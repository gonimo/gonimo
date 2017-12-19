{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Gonimo.Server.Db
Description : Server data base access.
Copyright   : (c) Robert Klotzner, 2017

db query events are handled by this module.
-}
module Gonimo.Server.Db ( -- * Types and classes
                              Db(..)
                            , HasDb(..)
                            -- * Functions
                            , runDb
                            , mayRunDb
                            , generateSecret
                            ) where


import           Control.Lens
import           Control.Logging.Extended
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                     (Text)
import           Database.Persist.Sql          (SqlBackend, runSqlPool)

import           Gonimo.Server.Db.Internal
import           Gonimo.SocketAPI.Model        (Secret(..))

make :: forall t m. MonadAppHost t m => Config t -> m (Db t)
make conf =  do
    (errorResponse, fireErrorResponse) <- newExternalEvent
    -- STM is a bit of an overkill, but if offers everything we need in an easy way. (Blocking read/write queue)
    queue <- liftIO newTQueueIO

    App.performEvent_ $ queueRequests queue <$> conf ^. onRequest
    forkIO $ processRequests queue fireErrorResponse

    let (_onError, _onResponse) = fanEither errorResponse
    pure $ Db {..}

queueRequests :: TQueue Request -> [Request] -> IO ()
queueRequests queue reqs = atomically $ traverse_ writeTQueue queue reqs

processRequests :: Server.Config -> TQueue Request
                -> (Either (Request, ServerError) Cache.ModelDump -> IO ())
                -> IO ()
processRequests conf queue sendResult = forever $ do
  next <- atomically $ readTQueue queue
  let
    -- Avoid empty messages:
    sendResult' msg = fromMaybe (pure ()) $ do
      success <- msg ^? _Right
      guard (success /= mempty)
      sendResult msg

    process :: IO Cache.ModelDump
    process =
      case next^.command of
        MakeFamily aid -> makeFamily aid
        MakeInvitation fid -> makeInvitation fid
        LoadInvitation secret -> loadInvitation secret
        MakeFamilyAccount fid aid invDelivery -> makeFamilyAccount fid aid invDelivery
        LoadAccount aid -> loadAccount aid
        Write action -> const mempty <$> runDb conf action

    safeProcess :: IO (Either (Request, ServerError) Cache.ModelDump)
    safeProcess = left (next,) <$> try process

  sendResult' =<< safeProcess
