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
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Gonimo.Db.Entities (DeviceId)
import           Reflex.Dom

import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Types                      (Secret)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configConnectBaby :: Event t DeviceId
            , _configDisconnectBaby :: Event t DeviceId
            , _configDisconnectAll :: Event t ()
            }

data Connections t
  = Connections { _request :: Event t [ API.ServerRequest ]
                , _streams :: Dynamic t (Map DeviceId MediaStream)
                }


makeLenses ''Config
makeLenses ''Connections

type ChannelsTransformation t = Map (API.ToId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

connections :: forall m t. MonadWidget t m => Config t -> m (Connections t)
connections config = mdo
  let
    ourDevId = API.deviceId <$> current (config^.configAuthData)
    openChannelReq = (:[]) <$> attachWith API.ReqCreateChannel ourDevId (config^.configConnectBaby)
  let
  (channelEvent, triggerChannelEvent) <- newTriggerEvent
  let
    makeChannel = push (\res -> do
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

                              pure . Just $ ((toId,secret),) <$> Channel.channel chanConfig
                            _ -> pure Nothing
                       )
                  (config^.configResponse)
  newChannel <- performEvent makeChannel
  let
    addChannel :: Event t [ChannelsTransformation t]
    addChannel = (:[]) . uncurry Map.insert <$> newChannel


  let
    getChannelsKey :: Event t DeviceId -> Event t (DeviceId, Secret)
    getChannelsKey devId = push (\devId' -> do
                                    cSecrets <- sample $ current secrets'
                                    pure $ (devId',) <$> cSecrets^.at devId'
                                ) devId
    closeEvent = leftmost [ const Channel.AllChannels <$> config^.configDisconnectAll
                          , Channel.OnlyChannel <$> getChannelsKey (config^.configDisconnectBaby)
                          ]
  let
    removeChannels :: Event t [ChannelsTransformation t]
    removeChannels = map (uncurry $ flip Map.update)
                     <$> Channel.getClosedChannels (current channels') (config^.configResponse) channelEvent closeEvent

  let applyActions = push (\actions -> do
                             cChannels <- sample $ current channels'
                             pure . Just $ foldr ($) cChannels actions
                         )

  channels' <- holdDyn Map.empty . applyActions $ mconcat [removeChannels, addChannel]
  let secrets' = Map.fromList . Map.keys <$> channels'

  let closeRequests = Channel.sendCloseMessages (current $ config^.configAuthData) (current channels') closeEvent
  Channel.closeRTCConnections (current channels') closeEvent

  channelRequests <- Channel.handleMessages (current $ config^.configAuthData) (current channels') (config^.configResponse)
  rtcRequests <- Channel.handleRTCEvents (current $ config^.configAuthData) (current channels') channelEvent

  channelStreams <- Channel.getRemoteStreams channelEvent
  let streams' = Map.fromList . (over (mapped._1) (^._1)) . Map.toList <$> channelStreams

  pure $ Connections { _request = openChannelReq <> channelRequests <> closeRequests <> startStreamingReq config <> rtcRequests
                     , _streams = streams'
                     }


startStreamingReq :: forall t. Reflex t => Config t -> Event t [API.ServerRequest]
startStreamingReq = push (\res -> case res of
                             API.ResCreatedChannel fromId toId secret -> pure . Just $ [API.ReqSendMessage fromId toId secret API.MsgStartStreaming]
                             _ -> pure Nothing
                         ) . _configResponse
