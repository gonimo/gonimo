{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Gonimo.Client.Baby.Socket where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Reflex.Dom

import           GHCJS.DOM.Types                   (MediaStream)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Types                      (Secret, DeviceType(..))
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configEnabled :: Dynamic t Bool
            , _configMediaStream :: Dynamic t MediaStream
            }

data Socket t
  = Socket { _request :: Event t [ API.ServerRequest ]
           , _channels :: Dynamic t (Map (API.FromId, Secret) (Channel t))
           , _subscriptions :: SubscriptionsDyn t
           }


makeLenses ''Config
makeLenses ''Socket

type ChannelsTransformation t = Map (API.FromId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

socket :: forall m t. MonadWidget t m => Config t -> m (Socket t)
socket config = mdo
  let
    gatedResponse = gate (current $ config^.configEnabled) (config^.configResponse)
  (channelEvent, triggerChannelEvent) <- newTriggerEvent
  let
    makeChannel = push (\res -> do
                          cStream <- sample $ current (config^.configMediaStream)
                          case res of
                            API.EventChannelRequested fromId secret
                              -> do
                              let chanConfig
                                    = Channel.Config { Channel._configResponse = gatedResponse
                                                     , Channel._configTriggerChannelEvent = triggerChannelEvent
                                                     , Channel._configSourceStream = Just cStream
                                                     , Channel._configTheirId = fromId
                                                     , Channel._configSecret = secret
                                                     }

                              pure . Just $ ((fromId, secret),) <$> Channel.channel chanConfig
                            _ -> pure Nothing
                       )
                  gatedResponse
  newChannel <- performEvent makeChannel
  let
    addChannel :: Event t [ChannelsTransformation t]
    addChannel = (:[]) . uncurry Map.insert <$> newChannel

  let
    closeEvent = push (\enabled ->
                         if enabled
                         then pure $ Nothing
                         else pure $ Just Channel.AllChannels
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

  let deviceTypeDyn = (\on -> if on then Baby "baby" else NoBaby) <$> config^.configEnabled
  let setDeviceType = zipDynWith API.ReqSetDeviceType (API.deviceId <$> config^.configAuthData) deviceTypeDyn
  let subs = Set.singleton <$> setDeviceType -- Dummy subscription, never changes but it will be re-executed when the connection breaks.
  pure $ Socket { _request = channelRequests <> closeRequests
                , _channels = channels'
                , _subscriptions = subs
                }
