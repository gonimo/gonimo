{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Gonimo.Client.Parent.Connections where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Gonimo.ConnectionsAPI                  as API
import qualified Gonimo.ConnectionsAPI.Types            as API
import           Reflex.Dom

import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Types                      (Secret, DeviceType(..))
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configConnectBaby :: Event t DeviceId
            , _configDisconnectBaby :: Event t DeviceId
            }

data Connections t
  = Connections { _request :: Event t [ API.ServerRequest ]
                , _subscriptions :: SubscriptionsDyn t
                , _streams :: Dynamic t (Map DeviceId MediaStream)
                }


makeLenses ''Config
makeLenses ''Connections

type ChannelsTransformation t = Map (API.FromId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

connections :: forall m t. MonadWidget t m => Config t -> m (Connections t)
connections config = mdo
  let
    ourDevId = API.deviceId <$> current (config^.configAuthData)
    openChannelReq = (:[]) <$> attachWith API.ReqCreateChannel ourDevId (config^.configConnectBaby)
  let
  (channelEvent, triggerChannelEvent) <- newTriggerEvent
  let
    makeChannel = push (\res -> do
                          cStream <- sample $ current (config^.configMediaStream)
                          case res of
                            API.ResCreatedChannel _ toId secret
                              -> do
                              let chanConfig
                                    = Channel.Config { Channel._configResponse = config^.configResponse
                                                     , Channel._configTriggerChannelEvent = triggerChannelEvent
                                                     , Channel._configSourceStream = Nothing
                                                     , Channel._configTheirId = toId
                                                     , Channel._configSecret = secret
                                                     }

                              pure . Just $ ((toId, secret),) <$> Channel.channel chanConfig
                            _ -> pure Nothing
                       )
                  (config^.configResponse)
  newChannel <- performEvent makeChannel
  let
    addChannel :: Event t [ChannelsTransformation t]
    addChannel = (:[]) . uncurry Map.insert <$> newChannel

  let
    closeEvent = push (\enabled ->
                         if enabled
                         then pure $ Nothing
                         else pure $ Just ()
                      ) (updated $ config^.configEnabled)
  let
    removeChannels :: Event t [ChannelsTransformation t]
    removeChannels = map Map.delete
                     <$> Channel.getClosedChannels (current channels') gatedResponse channelEvent closeEvent

  let applyActions = push (\actions -> do
                             cChannels <- sample $ current channels'
                             pure . Just $ foldr ($) cChannels actions
                         )

  channels' <- holdDyn Map.empty . applyActions $ mconcat [removeChannels, addChannel]

  let closeRequests = Channel.sendCloseMessages (current $ config^.configAuthData) (current channels') closeEvent

  channelRequests <- Channel.handleMessages (current $ config^.configAuthData) (current channels') gatedResponse

  let deviceTypeDyn = (\on -> if on then Parent "parent" else NoParent) <$> config^.configEnabled
  let setDeviceType = zipDynWith API.ReqSetDeviceType (API.deviceId <$> config^.configAuthData) deviceTypeDyn
  let subs = Set.singleton <$> setDeviceType -- Dummy subscription, never changes but it will be re-executed when the connection breaks.
  pure $ Connections { _request = openChannelReq <> channelRequests <> closeRequests
                , _channels = channels'
                , _subscriptions = subs
                }
