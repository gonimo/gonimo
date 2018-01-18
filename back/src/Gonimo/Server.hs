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
import qualified Gonimo.Server.Cache as Cache
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
  = Request { _message :: FromClient
            , _senderId :: DeviceId
            }

data RequestInfo
  = RequestInfo { _infoStatus :: Status
                , _infoRequest :: Request
                }

-- data RequestResult t
--   = RequestResult = { _responses :: Event t [(DeviceId, ToClient)]
--                     , _cacheUpdate :: Cache.Config t
--                     , _dbWrites :: Event t [ReaderT SqlBackend IO a]
--                     }

-- | Super type for 'RequestResult' which does not allow for db queries.
--
--   This type is produced by 'DbResponseHandler' in response to db results
--   triggered by the db accessing entries of 'RequestResult', like '_updates'
--   or '_dbRequests'. This type exists, because we don't want to allow db
--   results to trigger further db queries.
data DbRequestResult
  = DbRequestResult { _responses   :: [(DeviceId, ToClient)]
                    , _logMessages :: [(LogLevel, Text)]
                    }

-- | The result of a processed request.
--
--   The result of a processed request can either be direct '_responses' or
--   '_logMessages' or db queries (e.b. '_deletes') which trigger a further
--   'DbRequestResult' by means of their 'DbResponseHandler'.
data RequestResult
  = RequestResult { __dbRequestResult :: DbRequestResult
                  , _updates    :: [Db.UpdateRequest DbResponseHandler]
                  , _deletes    :: [Db.DeleteRequest DbResponseHandler]
                  , _dbRequests :: [Db.Request DbResponseHandler]
                  }

instance Monoid DbRequestResult where
  mempty = DbRequestResult [] []
  mappend (DbRequestResult r1 l1) (DbRequestResult r2 l2)
    = DbRequestResult (r1 <> r2) (l1 <> l2)

instance Monoid RequestResult where
  mempty = RequestResult mempty [] [] []
  mappend (RequestResult db1 u1 d1 r1) (RequestResult db1 u1 d1 r1)
    = RequestResult (db1 <> db2) (u1 <> u2) (d1 <> d2) (r1 <> r2)

type DbResponseHandler = Status -> Db.ErrorResult -> DbRequestResult
type DbResultHandler   = Status -> Db.Result -> DbRequestResult


fromDbResultHandler :: HasRequest r
  => r -- ^ The original request processed.
  -> Text -- ^ Error message to log in case of a database error.
  -> DbResultHandler -- ^ Handler for handling the success case.
  -> DbResponseHandler
fromDbResultHandler req errMsg h status' errResult
  = case errResult of
      Left err -> DbRequestResult { _responses   = [ (req ^. senderId, ServerError (req ^. message) err) ]
                                  , _logMessages = [ (LevelError, fullErrMsg) ]
                                  }
      Right r  -> h status' r
  where
    fullErrMsg = errMsg <> "\n  error: " <> (T.pack . show) err
                 <> "\n  offending requeset: " <> (T.pack .show) (r ^. message)
                 <> "\n  from client: " <> (T.pack . show)  (r ^. senderId)

-- | Default 'DbResponseHandler' if you don't need any additional information
--   from a database response.
mkDefaultDbHandler :: HasRequest r
  => r -- ^ The original request processed.
  -> Text -- ^ Error message to log in case of a database error.
  -> DbResponseHandler
mkDefaultDbHandler req errMsg = fromDbResultHandler req errMsg $ \status' result' ->
        case result' of
          Db.MadeFamily (fid, _) (_, famAcc) ->
            let
              aid = familyAccountAccountId famAcc
            in
              sendMessageAccount status' aid $ API.UpdateClient (API.OnNewFamilyAccount fid aid)
          Db.MadeInvitation invId inv -> 
            let
              fid = invitationFamilyId inv
              msg = API.UpdateClient (API.OnNewFamilyInvitation fid invId)
            in
              sendMessageFamily status' fid msg
          Db.MadeFamilyAccount _ famAcc ->
            let
              fid = familyAccountFamilyId famAcc
              aid = familyAccountAccountId fammAcc
              msg = API.UpdateClient (API.OnNewFamilyAccount fid aid)
            in
              sendMessageFamily status' fid msg <> sendMessageAccount status' aid msg
          Db.Wrote -> mempty
          Db.Loaded _ -> mempty

mkDeleteInvitationDbResultHandler :: InvitationId -> Invitation -> DbResultHandler
mkDeleteInvitationDbResultHandler invId inv status' _ =
    let
      famId = invitationFamilyId inv
      mAid = inv ^. to invitationReceiverId

      famMsg = API.Update (API.OnRemovedFamilyInvitation famId invId)
      accMsg = API.Update (API.OnRemovedAccountInvitation aid invId)
      mSendAccount = case mAid of
                       Nothing -> mempty
                       Just aid -> sendMessageAccount status' aid accMsg
    in
      sendMessageFamily status' famId famMsg <> mSendAccount

-- | Get all clients of a given account.
--
--   That is all devices that are online for a given account.
--
--   You can use this function if you have a message for all clients (online
--   devices) of a given account.
getAccountClients :: AccountId -> Status -> [DeviceId]
getAccountClients aid status' =
  let
    byAccountId' = status' ^. sampledModel . devices . to byAccountId
    devIds = byAccountId' ^. at aid . to Set.toList
    isOnline = map (\devId -> isJust $ status' ^. clients . at devId) devIds
  in
    map snd . filter fst $ zip isOnline devIds

-- | Get all clients that have selected a given family.
--
--   If you have a message that should be sent to all online members of a given
--   family you should use this function.
getFamilyClients :: FamilyId -> Status -> [DeviceId]
getFamilyClients fid status' = Statuses.getFamilyClient fid (status' ^. clients)


-- | Send a message to all online family members (that have the current family selected).
sendMessageFamily :: (HasDbRequestResult r, Monoid r, HasStatus s)
                  => s -> FamilyId -> ToClient -> r
sendMessageFamily status' fid msg =
  let
    receivers = getFamilyClients fid status'
  in
    mempty & responses .~ map (, msg) receivers

-- | Send a message to all clients of a given account.
sendMessageAccount :: (HasDbRequestResult r, Monoid r, HasStatus s)
                   => s -> AccountId -> ToClient -> r
sendMessageAccount status' aid msg =
  let
    receivers = getAccountClients aid status'
  in
    mempty & responses .~ map (, msg) receivers

-- Server:
make :: forall t m. MonadAppHost t m => Config -> m Session.Config
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

processRequest :: RequestInfo -> RequestResult
processRequest req
  = case req^.message of
    Ping                        -> pure mempty
    MakeDevice _                -> pure mempty
    Authenticate _              -> pure mempty
    MakeFamily                  -> makeFamily
    MakeInvitation fid          -> makeInvitation req fid
    ClaimInvitation secret'     -> claimInvitation req secret'
    AnswerInvitation invId repl -> answerInvitation req invId repl
    SendMessage toId msg        -> mempty & responses .~ [(toId, ReceiveMessage (req^.senderId) msg)]
    UpdateServer update'        -> performUpdate req update'
    Get view'                   -> denyView auth view'
-- Cache handling:
-- On authentication, load all data the device has access to: Account, Devices, Families, Accounts & Devices in those families, invitations from those families & claimed invitations. Don't assume those data is loaded in cache, but check and load everything that is not there. Important: Don't load anything that is already there, that would make the cache inconsistent!
-- When selecting a family everything is already there - no loading required.
-- That's a good compromise on performance and implementation complexity. A db access on authentication is required anyway, and if we restrict the numbers of devices per account/ accounts per family, families per account this should work well.
performUpdate :: RequestInfo -> Update -> RequestResult t
performUpdate req update =
  case update of
    OnChangedFamilyName         fid name
      -> let
           fullHandler = fromDbResultHandler req "Updating family name failed" handler
           handler status' _ = sendMessageFamily status' fid (API.Update update)

           updateCmd = Db.Update { Db._updateTable = families
                                 , Db._updateIndex = fid
                                 , Db._updatePerform = \fam -> fam { _familyName = name }
                                 }
         in
           mempty & updates .~ [ updateRequest fullHandler updateCmd ]
    OnChangedFamilyLastAccessed fid t
      -> mempty & logMessages .~ [ (LevelError, "OnChangedFamilyLastAccessed received from client!") ]
    OnNewFamilyMember           fid aid
      -> mempty & logMessages .~ [ (LevelError, "OnNewFamilyMember received from client!") ]
    OnRemovedFamilyMember fid aid
      -> let
           fullHandler = fromDbResultHandler req "Removing family member failed" handler
           handler status' _ = sendMessageFamily status' fid (API.Update update)
           mId = FamilyAccounts.findByFamilyAndAccountId fid aid (req ^. sampledModel . familyAccounts)

           mkDeleteCmd id' = Db.Delete { Db._deleteTable = familyAccount
                                       , Db._deleteIndex = id'
                                       }
         in
           mempty & updates .~ fromMaybe [] (deleteRequest fullHandler . mkDeleteCmd <$> mId)

    OnNewFamilyInvitation fid invId
      -> mempty & logMessages .~ [ (LevelError, "OnNewFamilyInvitation received from client!") ]
    OnRemovedFamilyInviation fid invId
      -> let
           fullHandler = fromDbResultHandler req "Removing family invitation failed" handler
           handler status' _ = sendMessageFamily status' fid (API.Update update)
           deleteCmd = Db.Delete { Db._deleteTable = invitations
                                 , Db._deleteIndex = invId
                                 }
         in
           mempty & updates .~ [deleteRequest fullHandler deleteCmd]
    OnNewAccountDevice          aid devId
      -> mempty & logMessages .~ [ (LevelError, "OnNewAccountDevice received from client!") ]
    OnRemovedAccountDevice      aid devId
      -> mempty & logMessages .~ [ (LevelError, "OnRemovedAccountDevice not yet implemented!") ]
    OnNewAccountInvitation      aid invid
      -> mempty & logMessages .~ [ (LevelError, "OnNewAccountInvitation received from client!") ]
    OnNewAccountFamily          aid fid
      -> mempty & logMessages .~ [ (LevelError, "OnNewAccountFamily received from client!") ]
    OnChangedDeviceName         devId name     ->
      -> let
           fullHandler = fromDbResultHandler req "Updating device name failed" handler
           handler status' _ = sendMessageFamily status' fid (API.Update update)

           updateCmd = Db.Update { Db._updateTable = devices
                                 , Db._updateIndex = devId
                                 , Db._updatePerform = \dev -> dev { _deviceName = name }
                                 }
         in
           mempty & updates .~ [ updateRequest fullHandler updateCmd ]

    OnChangedDeviceLastAccessed devId t
      -> mempty & logMessages .~ [ (LevelError, "OnChangedDeviceLastAccessed received from client!") ]
    OnChangedDeviceStatus       devId fid status ->


    OnClaimedInvitation         _           ->
    OnChangedInvitationDelivery invId _     ->


makeFamily :: RequestInfo -> RequestResult
makeFamily req = fromMaybe err $ do
    let sender = req ^. senderId
    aid <- req ^? sampledModel . devices . at sender . _Just . to deviceAccountId
    pure $ mempty & dbRequests .~ [Db.request (mkDefaultDbHandler req "makeFamily failed") (Db.MakeFamily aid)]
  where
    err = reportError req' InternalServerError "makeFamily: Could not find sending device!"

makeInvitation :: HasRequest r => r -> FamilyId -> RequestResult
makeInvitation req fid =
  let
    let sender = req ^. senderId
    command' = Db.MakeInvitation sender fid
  in
    mempty & dbRequests .~ [ Db.request (mkDefaultDbHandler req "makeInvitation failed") command' ]

claimInvitation :: HasRequest r => r -> Secret -> RequestResult
claimInvitation req secret' =
  let
    let sender = req ^. senderId
    command' = Db.Load claimInvitation'
    claimInvitation' = toDump <$> Invitation.claim secret'

    toDump invData = mempty & dumpedInvitations .~ [invData]
  in
    mempty & dbRequests .~ [ Db.request (mkDefaultDbHandler req "claimInvitation failed") command' ]

answerInvitation :: RequestInfo -> InviationId -> InvitationReply -> RequestResult
answerInvitation req invId repl = fromMaybe invNotFound $ do
  inv <- req ^. sampledModel ^? invitations . at invId
  aid <- inv ^. to invitationReceiverId

  let
    sender = req ^. senderId
    deleteInv' = Db.Delete { Db._deleteTable = invitations
                           , Db._deleteIndex = invId
                           }

    famId = invitationFamilyId inv
    joinFamily' = case repl of
      InvitationReject -> []
      InvitationAccept -> [ MakeFamilyAccount famId aid (Just $ invitationDelivery inv) ]

    deleteHandler = fromDbResultHandler req "Deleting invitation failed for answerInvitation"
                    $ mkDeleteInvitationDbResultHandler invId inv

    joinHandler = mkDefaultDbHandler req "answerInvitation, joinFamily failed"

  pure $ mempty & deletes .~ [ Db.deleteRequest deleteHandler deleteInv' ]
                & dbRequests .~ map (Db.request joinHandler) joinFamily'
  where
    err = API.ServerError (req ^. message) NoSuchInvitation
    invNotFound = mempty & responses .~ [(req ^. senderId, err)]
                         & logMessages .~ [LevelError, "Invitation requested by client (answerInvitation) was not found in cache or was not claimed before! (should never happen!)"]


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
