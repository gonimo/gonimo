{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Gonimo.Server where

import Control.Logging (LogLevel)

import           Gonimo.Server.Clients (Clients)
import qualified Gonimo.Server.Clients as Clients
import qualified Gonimo.Server.CachedDb as Db
import qualified Gonimo.Server.Db.Invitation as Invitation



-- | Serve. This is a Wai.Application sutiable for being run by Warp for example.
--
--   The given database is assumed to have all needed tables already created.
makeApplication :: Config -> IO Wai.Application
makeApplication config = do
  sessionConfig <- runSpiderHost $ make config
  pure $ Clients.serve sessionConfig

{- Tasks:
  - Accept connection
  - Authorize -> open session with DeviceId in memory (No need to store AccountId, as it can be looked up if needed.)
  - handle requests
-}

data Server t
  = Server { __config :: Config t
           , __clients :: Clients t
           , _cache :: Cache t
           , _sessionConfig :: Session.Config
           }

data Status
  = Status { _sampledModel :: Cache.Model
           , _clients :: ClientStatuses
           }

data Request
  = Request { __status :: Status
            , _clients :: ClientStatuses
            , _message :: FromClient
            , _senderId :: DeviceId
            }

-- data RequestResult t
--   = RequestResult = { _responses :: Event t [(DeviceId, ToClient)]
--                     , _cacheUpdate :: Cache.Config t
--                     , _dbWrites :: Event t [ReaderT SqlBackend IO a]
--                     }
data RequestResult
  = RequestResult { _responses  :: [(DeviceId, ToClient)]
                  , _updates    :: [Db.UpdateRequest ServerRequester]
                  , _deletes    :: [Db.DeleteRequest ServerRequester]
                  , _dbRequests :: [Db.Request ServerRequester]
                  , _logMessages   :: [LogLevel, Text]
                  }

instance Monoid RequestResult where
  mempty = RequestResult [] id []
  mappend (RequestResult r1 c1 db1) (RequestResult r2 c2 db2)
    = RequestResult (r1 <> r2) (c1 . c2) (db1 <> db2)

type ServerRequester = [API.Update]

-- Default requester if you don't need any additional information from a database response.
defaultRequester :: ServerRequester
defaultRequester = []

-- Server:
make :: forall t m. MonadAppHost t m => Config -> m ()
make config = build $ do
    (_sessionConfig, __clients) <- Clients.make clientConfig
    RequestResult _onSend cacheConfig <- processRequests
    _cache <- Cache.make cacheConfig
    pure $ Server {..}
  where
    build :: (Server t -> m Server t) -> m Session.Config
    build = fmap _sessionConig . mfix


processRequests :: forall t m. MonadAppHost t m => Server t -> RequestResult t
processRequests server' = undefined

processRequest :: Request -> RequestResult
processRequest req
  = case req^.message of
    Ping                     -> pure mempty
    MakeDevice _             -> pure mempty
    Authenticate _           -> pure mempty
    MakeFamily               -> makeFamily
    MakeInvitation fid       -> makeInvitation fid
    ClaimInvitation secret'  -> claimInvitation secret'
    AnswerInvitation invId repl -> answerInvitation invId repl
    SendMessage toId _       -> denyUnless (isOnlineInSameFamily clients' senderId toId)
    UpdateServer update'     -> denyUpdate auth update'
    Get view'                -> denyView auth view'
-- Cache handling:
-- On authentication, load all data the device has access to: Account, Devices, Families, Accounts & Devices in those families, invitations from those families & claimed invitations. Don't assume those data is loaded in cache, but check and load everything that is not there. Important: Don't load anything that is already there, that would make the cache inconsistent!
-- When selecting a family everything is already there - no loading required.
-- That's a good compromise on performance and implementation complexity. A db access on authentication is required anyway, and if we restrict the numbers of devices per account/ accounts per family, families per account this should work well.
performUpdate :: Update -> Model -> Model
performUpdate update =
  case update of
    OnChangedFamilyName         fid name       ->
    OnChangedFamilyLastAccessed fid t         ->
    OnNewFamilyMember           fid aid         ->
    OnRemovedFamilyMember fid aid           ->


    OnNewFamilyInvitation       fid invId         ->
    OnRemovedFamilyInviation fid invId      ->


    OnNewAccountDevice          aid devId         ->
    OnRemovedAccountDevice      aid devId         ->
    OnNewAccountInvitation      aid invid         ->
    OnNewAccountFamily          aid fid         ->
    OnChangedDeviceName         devId name     ->


    OnChangedDeviceLastAccessed devId t         ->
    OnChangedDeviceStatus       devId fid status ->


    OnClaimedInvitation         _           ->
    OnChangedInvitationDelivery invId _     ->


makeFamily :: Request -> RequestResult
makeFamily req' = fromMaybe err $ do
    aid <- req' ^? sampledModel . devices . at (req' ^. senderId) . _Just . to deviceAccountId
    pure $ mempty & dbRequests .~ [Db.request defaultRequester (Db.MakeFamily aid)]
  where
    err = reportError req' InternalServerError "makeFamily: Could not find sending device!"

makeInvitation :: Request -> FamilyId -> RequestResult
makeInvitation req' fid =
  let
    command' = Db.MakeInvitation (req' ^. senderId) fid
  in
    mempty & dbRequests .~ [ Db.request defaultRequester command' ]

claimInvitation :: Secret -> RequestResult
claimInvitation secret' =
  let
    command' = Db.Load claimInvitation'
    claimInvitation' = toDump <$> Invitation.claim secret'

    toDump invData = mempty & dumpedInvitations .~ [invData]
  in
    mempty & dbRequests .~ [ Db.request defaultRequester command' ]

answerInvitation :: Cache.Model -> InviationId -> InvitationReply -> RequestResult
answerInvitation model' invId repl = do
  let
    deleteInv' = Db.Delete { Db._deleteTable = invitations
                           , Db._deleteIndex = invId
                           }
  inv <- model' ^? invitations . at invId 
  aid <- inv ^. to invitationReceiverId

  let
    let famId = invitationFamilyId inv
    command' = MakeFamilyAccount  famId aid (Just $ invitationDelivery inv)

    deleteRequester = [ OnRemovedFamilyInvitation famId invId
                      , OnRemovedAccountInvitation aid invId
                      ]

  pure $ mempty & deletes .~ [ Db.deleteRequest deleteRequester deleteInv' ]
                & dbRequests .~ [ Db.request defaultRequester command' ]

-- | Handle database responses:
handleDbResponse :: Request -> Db.ErrorResult -> RequestResult
handleDbResponse req' (Left err) = reportError req' err "Some database command failed!"
handleDbResponse req' (Right resp) =
  case resp of
    Db.MadeFamily (fid, _) (_, famAcc) ->
      let
        aid = familyAccountAccountId famAcc
      in
        mempty & responses .~ [(req' ^. senderId, API.UpdateClient (API.OnNewFamilyAccount fid aid))]
    Db.MadeInvitation invId inv ->
      mempty & responses .~ [()]

-- | Send an error to the client and log a message.
reportError :: Request -- ^ The request that failed.
  -> ServerError -- ^ The error to report to the client.
  -> Text -- ^ Log message
  -> RequestResult
reportError req err msg =
  let
    response = API.ServerError (req ^. senderId) err
  in
    mempty & responses .~ [response]
           & logMessages .~ [ (LevelError, msg) ]
    
{--


-- Authorizer:

data Config t
  = Config { _onAuthorize :: Event t (DeviceId, FromClient)
           }

data Authorizer t
  = Authorizer { _onForbidden :: Event t (DeviceId, ToClient)
               , _onAuthorized :: Event t FromClient
               }
-- Cache:
data LoadOrRead
  = LoadFamily !FamilyId
  | LoadAccount !AccountId
  | Read !View




handleFamilyLoad :: Event t (DeviceId, ViewSelector) -> m ( Event t (DeviceId, View),
                                                          , Event t FamilyId
                                                          , Event t AccountId
                                                          )
handleFamilyLoad cache onLoad = do
    let onLoadOrRead = pushAlways loadOrRead onLoad
    let onFamilyLoaded = 
    familyWaitingLoad <- foldBehavior id Map.empty . mergeWith (.) $
      [ push (pure . fmap (\p -> traverse (Map.insert (p^._1)) (p^?_2._LoadFamily))) onLoadOrRead
      , Map.delete <$> cache^.onLoadedFamilyData
      ]
  where
    addToWaiting :: key -> val -> Map key (Set val) -> Map key (Set val)
    addToWaiting k v = at k.non Set.empty %~ Set.insert v

    removeFromWaiting :: key -> val -> Map key (Set val) -> Map key (Set val)
    removeFromWaiting k v = at k . non Set.empty %~ Set.delete v

    loadOrRead (deviceId, selector) = do
      accountOrFamily <- eitherFamilyAccount selector
      case accountOrFamily of
        Left famId -> do
          families <- sample $ cache^.families
          if Map.member famId families
            then Read . (deviceId, ) <$> getCachedSelectorData selector
            else pure $ LoadFamily famId
        Right accId -> do
          accounts <- sample $ cache^.accounts
          if Map.member accId accounts
            then Read . (deviceId, ) <$> getCachedSelectorData selector
            else pure $ LoadAccount accId
--}
