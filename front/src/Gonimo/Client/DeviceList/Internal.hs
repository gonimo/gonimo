{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.DeviceList.Internal where

import Reflex.Dom
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (DeviceListId)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Data.Maybe (isJust, isNothing)
import Safe (headMay)
import Data.List (sort)
import Gonimo.Client.Reflex
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)
type DeviceListMap = Map DeviceListId Db.DeviceList

type FamilyMembers = Map Family

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t API.AuthData
           , _configFamilyId :: Dynamic t FamilyId
           , _configRemoveAccount :: Event t AccountId
           , _configRenameDevice :: Event t DeviceId
           }

data DeviceList t
  = DeviceList { _accountIds :: Dynamic t [AccountId]
               , _deviceIds :: Dynamic t (Map AccountId DeviceId)
               , _devices :: Dynamic t (Map DeviceId API.DeviceInfo)
               , _subscriptions :: SubscriptionsDyn t
               , _request :: Event t [ API.ServerRequest ]
               }

makeLenses ''Config
makeLenses ''DeviceList

makeSubscriptions :: forall m t. (HasWebView m, MonadWidget t m) => Config t -> SubscriptionsDyn t
makeSubscriptions familyId' = do
    let
      (membersSubs, accoundIdsEv) = getMembersSubscriptions
    accountIds <- holdDyn [] accoundIdsEv

    (deviceIdsSubs, deviceIds) <- getDeviceIdsSubscription accountIds

    pure $ (zipDynWith Set.union familyIdsSubs familiesSubs
          , mFamilyIds
          )
  where
    getMembersSubscription :: (SubscriptionsDyn t, Event t [AccountId])
    getMembersSubscription =
      let
        subs = Set.singleton . API.ReqGetFamilyMembers $ config^.configFamilyId

        handleGotAccounts resp = case resp of
          API.ResGotFamilyMembers _ aids -> pure . Just $ aids
          _ -> pure Nothing
        gotAccountsEvent = push handleGotFamilies (config^.configResponse)
     in
       (subs, gotAccountsEvent)

getDeviceIdsSubscription :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                            => Dynamic t [AccountId]
                            -> m ( SubscriptionsDyn t
                                 , Dynamic t (Map AccountId (Dynamic t [DeviceId]))
                                 )
getDeviceIdsSubscription accountIds =
  let
    gotDeviceIdsEvent = push (\resp -> case resp of
                                  API.ResGotDevices aid dids -> pure . Just $ (aid, dids)
                                _ -> pure Nothing
                              ) (config^.configResponse)

    deviceIdsFullDyn :: Event t (Dynamic t (AccountId, [DeviceId]))
    deviceIdsFullDyn = waitForReady gotDeviceIdsEvent

    deviceIds = push (\fullDyn -> do
                        (aid, _) <- sample $ current fullDyn
                        pure . Just $ (aid, snd <$> fullDyn)
                      ) deviceIdsFullDyn

  in
    subscribeKeys accountIds API.ReqGetDevices deviceIds

getDeviceInfoSubscription :: forall t m. (MonadHold t m, Reflex t, MonadFix m)
                             => Dynamic t [DeviceId]
                             -> m (SubscriptionsDyn t
                                  , Dynamic t (Map DeviceId (Dynamic t DeviceInfo))
                                  )
getDeviceInfoSubscription deviceIds =
      let
        gotDeviceInfoEvent = push (\resp -> case resp of
                                     API.ResGotDeviceInfo did info -> pure . Just $ (did, info)
                                   _ -> pure Nothing
                                 ) (config^.configResponse)

        infoFullDyn :: Event t (Dynamic t (DeviceId, DeviceInfo))
        infoFullDyn = waitForReady gotDeviceInfoEvent

        deviceInfo = push (\fullDyn -> do
                            (did, _) <- sample $ current fullDyn
                            pure . Just $ (did, snd <$> fullDyn)
                         ) infoFullDyn

      in
        subscribeKeys accountIds API.ReqGetDeviceInfo deviceInfo
