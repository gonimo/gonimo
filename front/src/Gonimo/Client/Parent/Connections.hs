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
import           Reflex.Dom.Core

import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import qualified Gonimo.Client.WebRTC.Channels      as Channels
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Types                      (Secret)
import           Data.Maybe

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
    newChannelReq = push (\res -> do
                             case res of
                               API.ResCreatedChannel _ toId secret -> pure $ Just (toId, secret)
                               _ -> pure Nothing
                         ) (config^.configResponse)
  let
    getChannelsKey :: Event t DeviceId -> Event t (DeviceId, Secret)
    getChannelsKey devId = push (\devId' -> do
                                    cSecrets <- sample $ current secrets'
                                    pure $ (devId',) <$> cSecrets^.at devId'
                                ) devId
    closeEvent = leftmost [ const Channels.AllChannels <$> config^.configDisconnectAll
                          , Channels.OnlyChannel <$> getChannelsKey (config^.configDisconnectBaby)
                          ]
  let
    channelsConfig = Channels.Config { Channels._configResponse = config^.configResponse
                                     , Channels._configOurId = API.deviceId <$> config^.configAuthData
                                     , Channels._configBroadcastStream = constDyn Nothing
                                     , Channels._configCreateChannel = newChannelReq
                                     , Channels._configCloseChannel = closeEvent
                                     }
  channels' <- Channels.channels channelsConfig
  let secrets' = Map.fromList . Map.keys <$> channels'^.Channels.channelMap

  let streams' = Map.fromList . (over (mapped._1) (^._1)) . catMaybes . fmap sequence . Map.toList . fmap (^.Channel.theirStream) <$> channels'^.Channels.channelMap

  pure $ Connections { _request = channels'^.Channels.request <> openChannelReq
                     , _streams = streams'
                     }
