{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.DeviceList.Internal where

import Control.Monad (join)
import Reflex.Dom
import Control.Monad.Fix (MonadFix)
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (DeviceId, AccountId, FamilyId)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens
import Data.Set (Set)
import Gonimo.Client.Subscriber (subscribeKeys)
import Gonimo.Types (DeviceType)

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configFamilyId :: Dynamic t FamilyId
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           -- , _configRemoveAccount :: Event t AccountId
           -- , _configRenameDevice :: Event t (DeviceId, Text)
           }

data DeviceList t
  = DeviceList { _deviceIds :: Dynamic t (Map AccountId (Dynamic t [DeviceId]))
               , _deviceInfos :: Dynamic t (Map DeviceId (Dynamic t API.DeviceInfo))
               , _onlineDevices :: Dynamic t (Map DeviceId DeviceType)
               , _subscriptions :: SubscriptionsDyn t
               , _request :: Event t [ API.ServerRequest ]
               }

makeLenses ''Config
makeLenses ''DeviceList

deviceList :: forall m t. (HasWebView m, MonadWidget t m) => Config t -> m (DeviceList t)
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

    let
      allDeviceIds :: Dynamic t [DeviceId]
      allDeviceIds = join (mconcat . Map.elems <$> accountDeviceIds')

    (deviceInfoSubs, deviceInfos') <- getDeviceInfoSubscription config allDeviceIds
    onlineDevices' <- holdDyn Map.empty gotOnlineDevices
    pure $ DeviceList { _deviceIds = accountDeviceIds'
                      , _deviceInfos = deviceInfos'
                      , _onlineDevices = onlineDevices'
                      , _subscriptions = onlineSub
                                         <> membersSubs
                                         <> accountDeviceIdsSubs
                                         <> deviceInfoSubs
                      , _request = never
                      }

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

getDeviceInfoSubscription :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                             => Config t -> Dynamic t [DeviceId]
                             -> m (SubscriptionsDyn t
                                  , Dynamic t (Map DeviceId (Dynamic t API.DeviceInfo))
                                  )
getDeviceInfoSubscription config deviceIds' =
      let
        gotDeviceInfo = push (\resp -> case resp of
                                         API.ResGotDeviceInfo did info -> pure . Just $ (did, info)
                                         _ -> pure Nothing
                             ) (config^.configResponse)

      in
        subscribeKeys deviceIds' API.ReqGetDeviceInfo gotDeviceInfo
