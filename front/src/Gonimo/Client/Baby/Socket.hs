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
import           Reflex.Dom.Core

import           GHCJS.DOM.Types                   (MediaStream, liftJSM, MonadJSM)
import qualified GHCJS.DOM.MediaStream             as MediaStream
import           GHCJS.DOM.MediaStreamTrack        (mute)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import qualified Gonimo.Client.WebRTC.Channels     as Channels
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Types                      (Secret, DeviceType(..))
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)
import           Data.Maybe
import           GHCJS.DOM.EventM

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configEnabled :: Dynamic t DeviceType
            , _configMediaStream :: Dynamic t MediaStream
            }

data Socket t
  = Socket { _request :: Event t [ API.ServerRequest ]
           , _channels :: Dynamic t (Channels.ChannelMap t)
           , _subscriptions :: SubscriptionsDyn t
           }


makeLenses ''Config
makeLenses ''Socket

type ChannelsTransformation t = Map (API.FromId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

socket :: forall m t. MonadWidget t m => Config t -> m (Socket t)
socket config = mdo
  let
    isEnabled = (\dt -> case dt of
                    NoBaby -> False
                    Baby _ -> True) <$> config^.configEnabled
    gatedResponse = gate (current $ isEnabled) (config^.configResponse)

  -- Close connection when stream gets muted for some reason - so user can't take a still for live video.
  (streamMuted, triggerStreamMuted) <- newTriggerEvent
  cStream <- sample . current $ config^.configMediaStream
  handleMutedTracks (triggerStreamMuted Channels.AllChannels) cStream
  performEvent_
    $ handleMutedTracks (triggerStreamMuted Channels.AllChannels)
      <$> updated (config^.configMediaStream)

  let
    newChannelReq = push (\res -> do
                          case res of
                            API.EventChannelRequested fromId secret -> pure . Just $ (fromId, secret)
                            _ -> pure Nothing
                       ) gatedResponse
  let
    closeEvent = push (\enabled ->
                         if enabled
                         then pure $ Nothing
                         else pure $ Just Channels.AllChannels
                      ) (updated isEnabled)
  let
    channelsConfig = Channels.Config { Channels._configResponse = gatedResponse
                                     , Channels._configOurId = API.deviceId <$> config^.configAuthData
                                     , Channels._configBroadcastStream = Just <$> config^.configMediaStream
                                     , Channels._configCreateChannel = newChannelReq
                                     , Channels._configCloseChannel = leftmost [ closeEvent, streamMuted ]
                                     }
  channels' <- Channels.channels channelsConfig

  let deviceTypeDyn = config^.configEnabled
  let setDeviceType = zipDynWith API.ReqSetDeviceType (API.deviceId <$> config^.configAuthData) deviceTypeDyn
  let subs = Set.singleton <$> setDeviceType -- Dummy subscription, never changes but it will be re-executed when the connection breaks.
  pure $ Socket { _request = channels'^.Channels.request
                , _channels = channels'^.Channels.channelMap
                , _subscriptions = subs
                }


handleMutedTracks :: MonadJSM m => IO () -> MediaStream -> m ()
handleMutedTracks triggerClose stream = do
  tracks <- catMaybes <$> MediaStream.getTracks stream
  closeListener <- liftJSM . newListener $ liftIO triggerClose
  let addCloseListener (event', track) = liftJSM $ addListener track event' closeListener False
  traverse_ addCloseListener $ (,) <$> [mute] <*> tracks -- Previously also ended
  
  
