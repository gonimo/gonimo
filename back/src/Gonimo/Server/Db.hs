{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Lens
import           Control.Logging.Extended
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                     (Text)
import           Data.Time.Clock
import           Database.Persist.Sql          (SqlBackend, runSqlPool)
import           Reflex                        hiding (Request)
import           Reflex.Host.App               as App

import           Gonimo.Prelude
import           Gonimo.Server.Cache           (HasModelDump (..))
import qualified Gonimo.Server.Cache           as Cache
import           Gonimo.Server.Config          (runDb)
import qualified Gonimo.Server.Config          as Server
import qualified Gonimo.Server.Db.Account      as Account
import qualified Gonimo.Server.Db.Family       as Family
import           Gonimo.Server.Db.Internal
import           Gonimo.Server.Error
import           Gonimo.Server.NameGenerator
import           Gonimo.SocketAPI.Model        (Secret (..))
import           Gonimo.SocketAPI.Model        as API

make :: forall t m. MonadAppHost t m => Config t -> m (Db t)
make conf =  do
    (errorResponse, fireErrorResponse) <- newExternalEvent
    -- STM is a bit of an overkill, but if offers everything we need in an easy way. (Blocking read/write queue)
    queue <- liftIO newTQueueIO

    App.performEvent_ $ liftIO . queueRequests queue <$> conf ^. onRequest
    liftIO . forkIO $ processRequests conf queue (void . fireErrorResponse)

    let (_onError, _onResponse) = fanEither errorResponse
    pure $ Db {..}

queueRequests :: TQueue Request -> [Request] -> IO ()
queueRequests queue reqs = atomically $ traverse_ (writeTQueue queue) reqs

processRequests :: Server.HasConfig conf => conf -> TQueue Request
                -> (Either (Request, ServerError) Cache.ModelDump -> IO ())
                -> IO ()
processRequests conf queue sendResult = forever $ do
  next <- atomically $ readTQueue queue
  let
    -- Avoid empty messages:
    sendResult' msg = fromMaybe (pure ()) $ do
      success <- msg ^? _Right
      guard (success /= mempty)
      pure $ sendResult msg

    process :: IO Cache.ModelDump
    process =
      case next^.command of
        MakeFamily aid -> makeFamily conf aid
        MakeInvitation fid -> makeInvitation conf fid
        LoadInvitation secret -> loadInvitation conf secret
        MakeFamilyAccount fid aid invDelivery -> makeFamilyAccount conf fid aid invDelivery
        LoadAccount aid -> loadAccount conf aid
        Write action -> const mempty <$> runDb conf action

    safeProcess :: IO (Either (Request, ServerError) Cache.ModelDump)
    safeProcess = left (next,) <$> try process

  sendResult' =<< safeProcess

makeFamily :: Server.HasConfig c => c -> AccountId -> IO Cache.ModelDump
makeFamily conf aid = do
  now <- getCurrentTime
  let predPool = conf ^. Server.predicates
  let namePool = conf ^. Server.familyNames
  n <- generateFamilyName predPool namePool
  runDb conf $ do
    let family' = Family {
        API.familyName = n
      , familyCreated = now
      , familyLastAccessed = now
      , familyLastUsedBabyNames = []
    }

    fid <- Family.insert family'
    let familyAccount' = FamilyAccount {
        familyAccountAccountId = aid
      , familyAccountFamilyId = fid
      , familyAccountJoined = now
      , familyAccountInvitedBy = Nothing
    }

    famAid <- Account.joinFamily predPool familyAccount'
    pure $ mempty & dumpedFamilies       .~ [(fid, family')]
                  & dumpedFamilyAccounts .~ [(famAid, familyAccount')]
