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
                            , make
                            ) where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Reflex.Host.App               as App

import           Gonimo.Prelude
import           Gonimo.Server.Config          (runDb, generateSecret)
import qualified Gonimo.Server.Config          as Server
import qualified Gonimo.Server.Db.Account      as Account
import qualified Gonimo.Server.Db.Family       as Family
import qualified Gonimo.Server.Db.Invitation       as Invitation
import           Gonimo.Server.Db.Internal
import           Gonimo.Server.Error
import           Gonimo.Server.NameGenerator
import           Gonimo.SocketAPI.Model        as API

-- | Make a new 'Db' for handling database requests.
--
--   NOTE: Not this function spawns a thread which will run throughout the entire
--   lifetime of the program. This is fine for our purposes right now, but in
--   case you want to use this function for some short-lived purpose, this
--   should be fixed.
make :: forall a t m. MonadAppHost t m => Config a t -> m (Db a t)
make conf =  do
    (_onResponse, fireOnResponse) <- newExternalEvent
    -- STM is a bit of an overkill, but if offers everything we need in an easy way. (Blocking read/write queue)
    queue <- liftIO newTQueueIO

    App.performEvent_ $ liftIO . queueRequests queue <$> conf ^. onRequest
    void . liftIO . forkIO $ processRequests conf queue (void . fireOnResponse)

    pure $ Db {..}

-- | Queue requests for execution by 'processRequests'.
queueRequests :: TQueue (Request a) -> [Request a] -> IO ()
queueRequests queue reqs = atomically $ traverse_ (writeTQueue queue) reqs

-- | Process requests queued by 'queueRequests'.
processRequests :: forall a conf. Server.HasConfig conf
  => conf -> TQueue (Request a)
  -> ((a, Either ServerError Result) -> IO ())
  -> IO ()
processRequests conf queue sendResult = forever $ do
  next <- atomically $ readTQueue queue
  let
    process :: IO Result
    process =
      case next^.command of
        MakeFamily aid                     -> makeFamily conf aid
        MakeInvitation devId fid           -> makeInvitation conf devId fid
        MakeFamilyAccount fid aid delivery -> makeFamilyAccount conf fid aid delivery
        Load action                        -> Loaded      <$> runDb conf action
        Write action                       -> const Wrote <$> runDb conf action

    safeProcess :: IO (a, Either ServerError Result)
    safeProcess = (next^.requester,) <$> try process

  sendResult =<< safeProcess

-- | Family creation.
--
--   As families with no members are not allowed, a FamilyAccount with the given
--   'AccountId' will be created too.
makeFamily :: Server.HasConfig c => c -> AccountId -> IO Result
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
    pure $ MadeFamily (fid, family') (famAid, familyAccount')

-- | Create a new invitation for a given family.
makeInvitation :: Server.HasConfig c => c -> DeviceId -> FamilyId -> IO Result
makeInvitation conf senderId' fid = do
  now <- getCurrentTime
  isecret <- generateSecret conf
  let inv = Invitation {
      invitationSecret = isecret
    , invitationFamilyId = fid
    , invitationCreated = now
    , invitationDelivery = OtherDelivery
    , invitationSenderId = senderId'
    , invitationReceiverId = Nothing
  }
  iid <- runDb conf $ Invitation.insert inv
  pure $ MadeInvitation iid inv


-- | Pair an account with a family by creating the corresponding 'FamilyAccount' entry.
makeFamilyAccount
  :: Server.HasConfig c
  => c -> FamilyId -> AccountId -> Maybe InvitationDelivery
  -> IO Result
makeFamilyAccount conf fid aid delivery = do
  let predPool = conf ^. Server.predicates
  now <- getCurrentTime
  runDb conf $ do
      let famAccount = FamilyAccount {
          familyAccountAccountId = aid
        , familyAccountFamilyId = fid
        , familyAccountJoined = now
        , familyAccountInvitedBy = delivery
        }
      famAccId <- Account.joinFamily predPool famAccount
      pure $ MadeFamilyAccount famAccId famAccount
