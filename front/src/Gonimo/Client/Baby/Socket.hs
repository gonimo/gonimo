{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Gonimo.Client.Baby.Socket where

import           Gonimo.Client.Prelude


import           Data.Map                      (Map)
import qualified Data.Set                      as Set
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.MediaStream         as MediaStream
import           GHCJS.DOM.MediaStreamTrack    (mute)
import           GHCJS.DOM.Types               (MediaStream, MonadJSM, liftJSM)
import qualified GHCJS.DOM.Types               as JS

import           Gonimo.Client.Subscriber.Impl      (SubscriptionsDyn)
import           Gonimo.Client.WebRTC.Channel  (Channel)
import qualified Gonimo.Client.WebRTC.Channels as Channels
import qualified Gonimo.SocketAPI              as API
import qualified Gonimo.SocketAPI.Types        as API
import           Gonimo.Types                  (DeviceType (..), Secret)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configEnabled :: Dynamic t DeviceType
            , _configMediaStream :: Dynamic t (Either JS.PromiseRejected MediaStream)
            }

data Socket t
  = Socket { _request       :: Event t [ API.ServerRequest ]
           , _channels      :: Dynamic t (Channels.ChannelMap t)
           , _subscriptions :: SubscriptionsDyn t
           }


type ChannelsTransformation t = Map (API.FromId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

type HasModel model = Channels.HasModel model

socket :: forall model m t. (HasModel model, GonimoM model t m) => Config t -> m (Socket t)
socket config = mdo
  let
    isEnabled = (\dt -> case dt of
                    NoBaby -> False
                    Baby _ -> True) <$> config^.configEnabled
    gatedResponse = gate (current $ isEnabled) (config^.configResponse)

  -- Close connection when stream gets muted for some reason - so user can't take a still for live video.
  -- (streamMuted, triggerStreamMuted) <- newTriggerEvent
  -- cStream <- sample . current $ config^.configMediaStream
  -- handleMutedTracks (triggerStreamMuted Channels.AllChannels) cStream
  -- performEvent_
  --   $ handleMutedTracks (triggerStreamMuted Channels.AllChannels)
  --     <$> updated (config^.configMediaStream)

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
                                     , Channels._configBroadcastStream = either (const Nothing) Just <$> config^.configMediaStream
                                     , Channels._configCreateChannel = newChannelReq
                                     -- , Channels._configCloseChannel = leftmost [ closeEvent, streamMuted ]
                                     , Channels._configCloseChannel = leftmost [ closeEvent ]
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
  tracks <- MediaStream.getTracks stream
  closeListener <- liftJSM . newListener $ liftIO triggerClose
  let addCloseListener (event', track) = liftJSM $ addListener track event' closeListener False
  traverse_ addCloseListener $ (,) <$> [mute] <*> tracks -- Previously also ended



-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthData :: Lens' (Config t) (Dynamic t API.AuthData)
configAuthData f config' = (\configAuthData' -> config' { _configAuthData = configAuthData' }) <$> f (_configAuthData config')

configEnabled :: Lens' (Config t) (Dynamic t DeviceType)
configEnabled f config' = (\configEnabled' -> config' { _configEnabled = configEnabled' }) <$> f (_configEnabled config')

configMediaStream :: Lens' (Config t) (Dynamic t (Either JS.PromiseRejected MediaStream))
configMediaStream f config' = (\configMediaStream' -> config' { _configMediaStream = configMediaStream' }) <$> f (_configMediaStream config')


-- Lenses for Socket t:

request :: Lens' (Socket t) (Event t [ API.ServerRequest ])
request f socket' = (\request' -> socket' { _request = request' }) <$> f (_request socket')

channels :: Lens' (Socket t) (Dynamic t (Channels.ChannelMap t))
channels f socket' = (\channels' -> socket' { _channels = channels' }) <$> f (_channels socket')

subscriptions :: Lens' (Socket t) (SubscriptionsDyn t)
subscriptions f socket' = (\subscriptions' -> socket' { _subscriptions = subscriptions' }) <$> f (_subscriptions socket')


