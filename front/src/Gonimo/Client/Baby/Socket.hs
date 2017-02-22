{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Gonimo.Client.Baby.Socket where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Maybe (catMaybes)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Set                          ((\\))
import qualified Data.Set                          as Set
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import           Gonimo.Client.Server              (webSocket_recv)
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.Db.Entities                as Db
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Reflex.Dom

import           GHCJS.DOM.Types                   (MediaStream, MonadJSM)
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.Client.Auth                as Auth
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import           Gonimo.Client.WebRTC.Channel      (Channel)
import           Gonimo.Db.Entities                (DeviceId)
import           Gonimo.Types                      (Secret)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Event t API.AuthData
            , _configClose :: Event t ()
            , _configMediaStream :: Dynamic t MediaStream
            }

data Socket t
  = Socket { _request :: Event t [ API.ServerRequest ]
           , _channels :: Dynamic t (Map (API.FromId, Secret) (Channel t))
           }


makeLenses ''Config
makeLenses ''Socket

type ChannelsTransformation t = Map (API.FromId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

socket :: forall m t. (MonadFix m, MonadHold t m, MonadHold t (Performable m),
                        PerformEvent t m, PerformEvent t (Performable m), MonadJSM (Performable m),
                        MonadJSM (Performable (Performable m)), TriggerEvent t (Performable m))
        => Config t -> m (Socket t)
socket config = mdo
  let
    makeChannel = push (\res -> do
                          cStream <- sample $ current (config^.configMediaStream)
                          case res of
                            API.EventChannelRequested fromId secret
                              -> pure . Just $ ((fromId, secret),) <$> mkChannel config fromId secret cStream
                            _ -> pure Nothing
                       )
                  (config^.configResponse)
  newChannel <- performEvent makeChannel
  let
    addChannel :: Event t [ChannelsTransformation t]
    addChannel = (:[]) . uncurry Map.insert <$> newChannel

  let
    getRemoveEvent :: ((API.FromId, Secret), Channel t) -> Event t [(API.FromId, Secret)]
    getRemoveEvent (key, chan) = const [key] <$> (chan^.Channel.closed)
  let
    dynRemoveEvents :: Dynamic t (Event t [(API.FromId, Secret)])
    dynRemoveEvents = mconcat . map getRemoveEvent . Map.toList <$> channels'
  let
    removeChannels :: Event t [ChannelsTransformation t]
    removeChannels = map Map.delete <$> switchPromptlyDyn dynRemoveEvents

  let applyActions = push (\actions -> do
                             cChannels <- sample $ current channels'
                             pure . Just $ foldr ($) cChannels actions
                         )

  channels' <- holdDyn Map.empty . applyActions $ mconcat [removeChannels, addChannel]

  let dynChannelRequests = mconcat . map Channel._request . Map.elems <$> channels'
  let channelRequests = switchPromptlyDyn dynChannelRequests
  pure $ Socket { _request = channelRequests
                , _channels = channels'
                }


mkChannel :: forall m t. (MonadHold t m, TriggerEvent t m, MonadJSM (Performable m), MonadJSM m, PerformEvent t m) => Config t -> DeviceId -> Secret -> MediaStream -> m (Channel t)
mkChannel config remoteId secret stream
  = Channel.channel
    $ Channel.Config { Channel._configResponse = config^.configResponse
                     , Channel._configSourceStream = Just stream
                     , Channel._configOurId = API.deviceId $ config^.configAuthData
                     , Channel._configTheirId = remoteId
                     , Channel._configSecret = secret
                     , Channel._configClose = config^.configClose
                     }
