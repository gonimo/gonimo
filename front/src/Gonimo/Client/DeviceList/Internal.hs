{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.DeviceList.Internal where

import           Control.Lens
import           Control.Monad.Fix        (MonadFix)
import           Data.Map                 (Map)
import           Data.Text                 (Text)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Gonimo.Client.Reflex     (buildMap)
import           Gonimo.Client.Subscriber (subscribeKeys)
import           Gonimo.SocketAPI.Types       (AccountId, DeviceId, FamilyId)
import qualified Gonimo.SocketAPI         as API
import qualified Gonimo.SocketAPI.Types   as API
import           Gonimo.Types             (DeviceType)
import           Reflex.Dom.Core
import           Gonimo.Client.Prelude
import           Data.Default

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)
type NestedDeviceInfos t = Map AccountId (Dynamic t (Map DeviceId (Dynamic t API.DeviceInfo)))

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configFamilyId :: Dynamic t FamilyId
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           -- , _configRemoveAccount :: Event t AccountId
           -- , _configRenameDevice :: Event t (DeviceId, Text)
           }

data DeviceList t
  = DeviceList { _deviceInfos :: Dynamic t (NestedDeviceInfos t)
               , _onlineDevices :: Dynamic t (Map DeviceId DeviceType)
               , _subscriptions :: SubscriptionsDyn t
               , _request :: Event t [ API.ServerRequest ]
               }

data UI t
  = UI { _uiRequest :: Event t [ API.ServerRequest ]
       , _uiConnect :: Event t DeviceId
       , _uiDisconnect :: Event t DeviceId
       , _uiShowStream :: Event t DeviceId
       }

instance Reflex t => Default (UI t) where
  def = UI { _uiRequest = never
           , _uiConnect = never
           , _uiDisconnect = never
           , _uiShowStream = never
           }

uiSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (UI t) -> m (UI t)
uiSwitchPromptly ev
  = UI <$> switchPromptly never (_uiRequest <$> ev)
       <*> switchPromptly never (_uiConnect <$> ev)
       <*> switchPromptly never (_uiDisconnect <$> ev)
       <*> switchPromptly never (_uiShowStream <$> ev)

uiLeftmost :: forall t. Reflex t => [UI t] -> UI t
uiLeftmost uis
  = UI (leftmost . map _uiRequest $ uis)
       (leftmost . map _uiConnect $ uis)
       (leftmost . map _uiDisconnect $ uis)
       (leftmost . map _uiShowStream $ uis)

ownDeviceName :: forall t. Reflex t => Dynamic t API.AuthData -> DeviceList t -> Dynamic t Text
ownDeviceName auth deviceList' = fmap (fromMaybe "") . runMaybeT $ do
  aid <- lift $ API.accountId <$> auth
  did <- lift $ API.deviceId <$> auth
  deviceIds <- MaybeT $ Map.lookup aid <$> (deviceList'^.deviceInfos)
  deviceInfo <- MaybeT $ Map.lookup did <$> deviceIds
  lift $ API.deviceInfoName <$> deviceInfo


deviceList :: forall m t. GonimoM t m => Config t -> m (DeviceList t)
deviceList config = do
    let
      onlineSub = Set.singleton . API.ReqGetOnlineDevices <$> config^.configFamilyId
      gotOnlineDevices = push (\resp -> case resp of
                                  API.ResGotOnlineDevices _ devList -> pure $ Just (Map.fromList devList)
                                  _ -> pure Nothing ) (config^.configResponse)

    let
      (membersSubs, accoundIdsEv) = getMembersSubscription config
    accountIds' <- holdDyn [] accoundIdsEv

    (accountDeviceIdsSubs, accountDeviceIds') <- getDeviceIdsSubscription config accountIds'

    let deviceInfoSubs = getDeviceInfosSubscription accountDeviceIds'
    -- deviceInfos' <- holdBackUntilReady accountDeviceIds' =<< getDeviceInfos config accountDeviceIds'
    deviceInfos' <- getDeviceInfos config accountDeviceIds'


    onlineDevices' <- holdDyn Map.empty gotOnlineDevices
    pure $ DeviceList { _deviceInfos = deviceInfos'
                      , _onlineDevices = onlineDevices'
                      , _subscriptions = onlineSub
                                         <> membersSubs
                                         <> accountDeviceIdsSubs
                                         <> deviceInfoSubs
                      , _request = never
                      }
  -- where
  --   -- Causes causality loop - why?
  --   holdBackUntilReady :: Dynamic t (Map AccountId (Dynamic t [DeviceId]))
  --                      -> Dynamic t (NestedDeviceInfos t) -> m (Dynamic t (NestedDeviceInfos t))
  --   holdBackUntilReady devIds infos = do
  --     let
  --       devIds' = joinDynThroughMap devIds
  --       infos' = joinDynThroughMap infos
  --       flattenedIdsIn = sort . concat . Map.elems <$> devIds'
  --       flattenedIdsOut = sort . concat . map Map.keys . Map.elems <$> infos'
  --       ourGate = zipDynWith (==) flattenedIdsIn flattenedIdsOut
  --       gated = (,) <$> ourGate <*> infos
  --       nextVal = push (\(newGate, newInfos) -> do
  --                          if newGate
  --                            then pure $ Just newInfos
  --                            else pure Nothing
  --                      ) (updated gated)
  --     initVal <- sample $ current infos
  --     holdDyn initVal nextVal


getMembersSubscription :: Reflex t => Config t -> (SubscriptionsDyn t, Event t [AccountId])
getMembersSubscription config =
  let
    subs = Set.singleton . API.ReqGetFamilyMembers <$> config^.configFamilyId

    handleGotAccounts resp = case resp of
      API.ResGotFamilyMembers _ aids -> pure . Just $ aids
      _ -> pure Nothing
    gotAccountsEvent = push handleGotAccounts (config^.configResponse)
  in
    (subs, gotAccountsEvent)

getDeviceIdsSubscription :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                            => Config t -> Dynamic t [AccountId]
                            -> m ( SubscriptionsDyn t
                                 , Dynamic t (Map AccountId (Dynamic t [DeviceId]))
                                 )
getDeviceIdsSubscription config accountIds' =
  let
    gotDeviceIds = push (\resp -> case resp of
                                    API.ResGotDevices aid dids -> pure . Just $ (aid, dids)
                                    _ -> pure Nothing
                        ) (config^.configResponse)

  in
    subscribeKeys accountIds' API.ReqGetDevices gotDeviceIds

getDeviceInfos :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                             => Config t -> Dynamic t (Map AccountId (Dynamic t [DeviceId]))
                             -> m (Dynamic t (NestedDeviceInfos t))
getDeviceInfos config deviceIds' = mdo
  let
    buildMapIncremental :: forall m1. (Monad m1, MonadHold t m1, MonadFix m1)
                           => Map AccountId (Dynamic t [DeviceId]) -> Map AccountId (Dynamic t [DeviceId]) -> m1 (Maybe (NestedDeviceInfos t -> NestedDeviceInfos t))
    buildMapIncremental oldDevIds newDevIds = do
        let created = Map.difference newDevIds oldDevIds
        let deleted = Map.difference oldDevIds newDevIds
        newDevInfos <- traverse (getAccountDeviceInfos config) created
        if (not . Map.null) created || (not . Map.null) deleted
          then pure . Just $ (`Map.difference` deleted) . (<> newDevInfos)
          else pure Nothing

  let
    updates :: Event t (Map AccountId (Dynamic t (Map DeviceId (Dynamic t API.DeviceInfo))))
    updates = push (\newDevIds -> do
                       oldDevIds <- sample $ current deviceIds'
                       doUpdate <-  buildMapIncremental oldDevIds newDevIds
                       oldResult <- sample $ current result
                       pure $ doUpdate <*> pure oldResult
                   ) (updated deviceIds')

  initInput <- sample $ current deviceIds'
  mkInitVal <- buildMapIncremental Map.empty initInput
  result <- holdDyn (fromMaybe Map.empty (mkInitVal <*> pure Map.empty)) updates
  pure result

getAccountDeviceInfos :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                             => Config t -> Dynamic t [DeviceId]
                             -> m (Dynamic t (Map DeviceId (Dynamic t API.DeviceInfo)))
getAccountDeviceInfos config deviceIds' =
      let
        gotDeviceInfo = push (\resp -> case resp of
                                         API.ResGotDeviceInfo did info -> pure . Just $ (did, info)
                                         _ -> pure Nothing
                             ) (config^.configResponse)

      in
        buildMap deviceIds' gotDeviceInfo

getDeviceInfosSubscription :: Reflex t => Dynamic t (Map AccountId (Dynamic t [DeviceId])) -> SubscriptionsDyn t
getDeviceInfosSubscription
  = fmap (mconcat . map getAccountDeviceInfosSubscription . Map.elems) . joinDynThroughMap

getAccountDeviceInfosSubscription :: [DeviceId] -> Set API.ServerRequest
getAccountDeviceInfosSubscription = Set.unions . map (Set.singleton . API.ReqGetDeviceInfo)


-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configFamilyId :: Lens' (Config t) (Dynamic t FamilyId)
configFamilyId f config' = (\configFamilyId' -> config' { _configFamilyId = configFamilyId' }) <$> f (_configFamilyId config')

configAuthData :: Lens' (Config t) (Dynamic t (Maybe API.AuthData))
configAuthData f config' = (\configAuthData' -> config' { _configAuthData = configAuthData' }) <$> f (_configAuthData config')


-- Lenses for DeviceList t:

deviceInfos :: Lens' (DeviceList t) (Dynamic t (NestedDeviceInfos t))
deviceInfos f deviceList' = (\deviceInfos' -> deviceList' { _deviceInfos = deviceInfos' }) <$> f (_deviceInfos deviceList')

onlineDevices :: Lens' (DeviceList t) (Dynamic t (Map DeviceId DeviceType))
onlineDevices f deviceList' = (\onlineDevices' -> deviceList' { _onlineDevices = onlineDevices' }) <$> f (_onlineDevices deviceList')

subscriptions :: Lens' (DeviceList t) (SubscriptionsDyn t)
subscriptions f deviceList' = (\subscriptions' -> deviceList' { _subscriptions = subscriptions' }) <$> f (_subscriptions deviceList')

request :: Lens' (DeviceList t) (Event t [ API.ServerRequest ])
request f deviceList' = (\request' -> deviceList' { _request = request' }) <$> f (_request deviceList')


-- Lenses for UI t:

uiRequest :: Lens' (UI t) (Event t [ API.ServerRequest ])
uiRequest f uI' = (\uiRequest' -> uI' { _uiRequest = uiRequest' }) <$> f (_uiRequest uI')

uiConnect :: Lens' (UI t) (Event t DeviceId)
uiConnect f uI' = (\uiConnect' -> uI' { _uiConnect = uiConnect' }) <$> f (_uiConnect uI')

uiDisconnect :: Lens' (UI t) (Event t DeviceId)
uiDisconnect f uI' = (\uiDisconnect' -> uI' { _uiDisconnect = uiDisconnect' }) <$> f (_uiDisconnect uI')

uiShowStream :: Lens' (UI t) (Event t DeviceId)
uiShowStream f uI' = (\uiShowStream' -> uI' { _uiShowStream = uiShowStream' }) <$> f (_uiShowStream uI')


