{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Gonimo.Server where


import           Gonimo.Server.Clients (Clients)
import qualified Gonimo.Server.Clients as Clients



-- | Serve. This is a Wai.Application sutiable for being run by Warp for example.
--
--   The given database is assumed to have all needed tables already created.
serve :: Config -> Wai.Application
serve config req respond = do
  (sessionConfig, clients) <- runSpiderHost $ Clients.make config
  Clients.serve sessionConfig req respond

{- Tasks:
  - Accept connection
  - Authorize -> open session with DeviceId in memory (No need to store AccountId, as it can be looked up if needed.)
  - handle requests
-}

data Server
  = Server { _
    
           }

-- Server:
make :: MonadAppHost t m => Config -> m ()
make config = do
  clients <- Clients.make clientConfig
  authorized <- Authorizer.make authConfig
  authorized >>= requesthandler

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
