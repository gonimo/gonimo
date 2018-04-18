{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Gonimo.Client.Parent.Connections where

import           Gonimo.Client.Prelude


import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import qualified GHCJS.DOM.AudioBufferSourceNode   as AudioNode
import           GHCJS.DOM.Types                   (MediaStream, MonadJSM,
                                                    liftJSM)
import qualified Language.Javascript.JSaddle.Value as JS

import qualified Gonimo.Client.NavBar              as NavBar
import           Gonimo.Client.Util                (boostMediaStreamVolume,
                                                    getVolumeInfo, loadSound,
                                                    startVibraAlert,
                                                    stopVibraAlert)
import           Gonimo.Client.WebRTC.Channel      (Channel)
import qualified Gonimo.Client.WebRTC.Channel      as Channel
import qualified Gonimo.Client.WebRTC.Channels     as Channels
import qualified Gonimo.SocketAPI                  as API
import           Gonimo.SocketAPI.Types            (DeviceId)
import qualified Gonimo.SocketAPI.Types            as API
import           Gonimo.Types                      (Secret)

data Config t
  = Config  { _configResponse       :: Event t API.ServerResponse
            , _configAuthData       :: Dynamic t API.AuthData
            , _configConnectBaby    :: Event t DeviceId
            , _configDisconnectBaby :: Event t DeviceId
            , _configDisconnectAll  :: Event t ()
            }

data Connections t
  = Connections { _request :: Event t [ API.ServerRequest ]
                , _channelMap :: Dynamic t (Map DeviceId (Channel.Channel t))
                , _origStreams :: Dynamic t (Map DeviceId MediaStream) -- neccessary to work around a bug in Chrome.
                , _streams :: Dynamic t (Map DeviceId (StreamData t))
                , _unreliableConnections :: Dynamic t Bool
                , _brokenConnections :: Dynamic t Bool
                }


data StreamData t
  = StreamData { _stream      :: MediaStream
               , _volumeLevel :: Event t Double
               }

data VideoView t
  = VideoView { _videoViewNavBar         :: NavBar.NavBar t
              , _videoViewDisconnectBaby :: Event t DeviceId
              , _videoViewDisconnectAll  :: Event t ()
              }

type HasModel model = Channels.HasModel model

type ChannelsTransformation t = Map (API.ToId, Secret) (Channel t) -> Map (API.FromId, Secret) (Channel t)

connections :: forall model m t. (HasModel model, GonimoM model t m) => Config t -> m (Connections t)
connections config = mdo
  let
    ourDevId = API.deviceId <$> current (config^.configAuthData)
    openChannelReq = (:[]) <$> attachWith API.ReqCreateChannel ourDevId (config^.configConnectBaby)
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
                          , Channels.OnlyChannel <$> getChannelsKey (leftmost [ config^.configDisconnectBaby
                                                                              , config^.configConnectBaby
                                                                              ]
                                                                    )
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

  let streams' = kickSecret <$> channels'^.Channels.remoteStreams
  (volumeEvent, triggerVolumeEvent) <- newTriggerEvent
  boostedStreams <- traverseCache (handleVolume triggerVolumeEvent volumeEvent) (updated streams') -- updated should be fine, as at startup there should be no streams.

  playAlarmOnBrokenConnection channels'

  unreliableConnections' <- areThereUnreliableConnections channels'
  brokenConnections'     <- areThereBrokenConnections channels'

  pure $ Connections { _request = channels'^.Channels.request <> openChannelReq
                     , _channelMap = kickSecret <$> channels'^.Channels.channelMap
                     , _origStreams = streams'
                     , _streams = boostedStreams
                     , _unreliableConnections = unreliableConnections'
                     , _brokenConnections = brokenConnections'
                     }
  where
    kickSecret :: forall v. Map (DeviceId, Secret) v -> Map DeviceId v
    kickSecret = Map.fromList . over (mapped._1) (^._1) . Map.toList

playAlarmOnBrokenConnection :: GonimoM model t m => Channels.Channels t -> m ()
playAlarmOnBrokenConnection channels' = mdo
    let
      loadAlert :: forall m1. MonadJSM m1 => m1 AudioNode.AudioBufferSourceNode
      loadAlert = loadSound "/sounds/gonimo_alarm_64kb_long.mp3"
    alarmSound <- loadAlert
    newAlertEv <- performEvent $ const loadAlert <$> ffilter not  (updated anyConnectionBroken) -- alarm can only be played once!
    -- AudioNode.start alarmSound 0 0 1000000 -- 0 for duration does not work on Chrome at least! fs
    anyConnectionBroken <- holdUniqDyn $ getAnyBrokenConnections <$> channels'^.Channels.channelMap

    alarmSoundBeh <- hold alarmSound newAlertEv
    performEvent_ $ startStopSound <$> attach alarmSoundBeh (traceEventWith show (updated anyConnectionBroken))

    vibra <- performEvent $ startStopVibra <$> attach vibraBeh (updated anyConnectionBroken)
    vibraBeh <- hold Nothing vibra

    pure ()
  where
    startStopVibra (mVibra, onOff)
      =  if onOff
         then Just <$> startVibraAlert
         else case mVibra of
                Nothing -> pure Nothing
                Just vibra -> do
                  stopVibraAlert vibra
                  pure Nothing

    startStopSound (sound, onOff)
      = if onOff
        then AudioNode.start sound (Just 0) (Just 0) (Just 1000) -- 0 for duration does not work on Chrome at least! fs
        else AudioNode.stop  sound (Just 0)

areThereBrokenConnections :: (Reflex t, MonadHold t m, MonadFix m) => Channels.Channels t -> m (Dynamic t Bool)
areThereBrokenConnections = holdUniqDyn . fmap getAnyBrokenConnections . (^.Channels.channelMap)

areThereUnreliableConnections :: (Reflex t, MonadHold t m, MonadFix m) => Channels.Channels t -> m (Dynamic t Bool)
areThereUnreliableConnections = holdUniqDyn . fmap getAnyUnreliableConnections . (^.Channels.channelMap)

getAnyBrokenConnections :: Channels.ChannelMap t -> Bool
getAnyBrokenConnections = any Channel.needsAlert . Map.elems

getAnyUnreliableConnections :: Channels.ChannelMap t -> Bool
getAnyUnreliableConnections = getConnectionsInState Channel.StateUnreliable

getConnectionsInState :: Channel.ReceivingState -> Channels.ChannelMap t -> Bool
getConnectionsInState state = any isChanInState . Map.elems
  where
    isChanInState chan = chan^.Channel.audioReceivingState == state
                         || chan^.Channel.videoReceivingState == state


handleVolume :: (Reflex t, MonadJSM m) => ((DeviceId, Double) -> IO ()) -> Event t (DeviceId, Double) -> DeviceId -> MediaStream -> m (StreamData t)
handleVolume triggerVolumeEvent volEvent key stream' = do
  boosted <- boostMediaStreamVolume stream'
  void $ getVolumeInfo stream' (liftIO . triggerVolumeEvent . (key,))
  let ourVolEvent = snd <$> ffilter (\(k, _) -> k == key) volEvent
  pure $ StreamData boosted ourVolEvent


-- A special traverse function which does not reapply the effectful function if an element stayed the same.
traverseCache :: forall m k t. ( MonadJSM m, Ord k, Reflex t, PerformEvent t m
                               , MonadJSM (Performable m), MonadFix m
                               , MonadHold t m
                               )
                 => (k -> MediaStream -> (Performable m) (StreamData t))
              -> Event t (Map k MediaStream)
              -> m (Dynamic t (Map k (StreamData t)))
traverseCache f inStreams = mdo
  let traverseEv = pushAlways (\newStreams -> do
                                  cCache <- sample cache
                                  pure $ pTraverseCache cCache f newStreams
                              ) inStreams
  newResult <- performEvent traverseEv
  results <- holdDyn Map.empty $ fst <$> newResult
  cache <- hold Map.empty $ snd <$> newResult
  pure results


pTraverseCache :: forall m k t. (MonadJSM m, Ord k)
                  => Map k (MediaStream,StreamData t)
               -> (k -> MediaStream -> m (StreamData t))
               -> Map k MediaStream
               -> m (Map k (StreamData t), Map k (MediaStream, (StreamData t)))
pTraverseCache cache f inStreams = do
  let
    possiblyCached = Map.intersection cache inStreams
    checkReallyCached k (sourceStream,_) = liftJSM $ do
      mR <- traverse (JS.strictEqual sourceStream) (inStreams^.at k)
      case mR of
        Nothing -> pure False
        Just r  -> pure r
  reallyCachedBool <- Map.traverseWithKey checkReallyCached possiblyCached
  let
    reallyCached :: Map k (MediaStream, StreamData t)
    reallyCached = Map.intersection possiblyCached reallyCachedBool

    toProcess = Map.difference inStreams reallyCached
  newEntries <- Map.traverseWithKey f toProcess
  let
    newCache = reallyCached <> Map.intersectionWith (\s d -> (s,d)) toProcess newEntries
  pure (snd <$> newCache, newCache)

-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthData :: Lens' (Config t) (Dynamic t API.AuthData)
configAuthData f config' = (\configAuthData' -> config' { _configAuthData = configAuthData' }) <$> f (_configAuthData config')

configConnectBaby :: Lens' (Config t) (Event t DeviceId)
configConnectBaby f config' = (\configConnectBaby' -> config' { _configConnectBaby = configConnectBaby' }) <$> f (_configConnectBaby config')

configDisconnectBaby :: Lens' (Config t) (Event t DeviceId)
configDisconnectBaby f config' = (\configDisconnectBaby' -> config' { _configDisconnectBaby = configDisconnectBaby' }) <$> f (_configDisconnectBaby config')

configDisconnectAll :: Lens' (Config t) (Event t ())
configDisconnectAll f config' = (\configDisconnectAll' -> config' { _configDisconnectAll = configDisconnectAll' }) <$> f (_configDisconnectAll config')


-- Lenses for Connections t:

request :: Lens' (Connections t) (Event t [ API.ServerRequest ])
request f connections' = (\request' -> connections' { _request = request' }) <$> f (_request connections')

channelMap :: Lens' (Connections t) (Dynamic t (Map DeviceId (Channel.Channel t)))
channelMap f connections' = (\channelMap' -> connections' { _channelMap = channelMap' }) <$> f (_channelMap connections')

origStreams :: Lens' (Connections t) (Dynamic t (Map DeviceId MediaStream))
origStreams f connections' = (\origStreams' -> connections' { _origStreams = origStreams' }) <$> f (_origStreams connections')

streams :: Lens' (Connections t) (Dynamic t (Map DeviceId (StreamData t)))
streams f connections' = (\streams' -> connections' { _streams = streams' }) <$> f (_streams connections')

unreliableConnections :: Lens' (Connections t) (Dynamic t Bool)
unreliableConnections f connections' = (\unreliableConnections' -> connections' { _unreliableConnections = unreliableConnections' }) <$> f (_unreliableConnections connections')

brokenConnections :: Lens' (Connections t) (Dynamic t Bool)
brokenConnections f connections' = (\brokenConnections' -> connections' { _brokenConnections = brokenConnections' }) <$> f (_brokenConnections connections')


-- Lenses for StreamData t:

stream :: Lens' (StreamData t) MediaStream
stream f streamData' = (\stream' -> streamData' { _stream = stream' }) <$> f (_stream streamData')

volumeLevel :: Lens' (StreamData t) (Event t Double)
volumeLevel f streamData' = (\volumeLevel' -> streamData' { _volumeLevel = volumeLevel' }) <$> f (_volumeLevel streamData')


-- Lenses for VideoView t:

videoViewNavBar :: Lens' (VideoView t) (NavBar.NavBar t)
videoViewNavBar f videoView' = (\videoViewNavBar' -> videoView' { _videoViewNavBar = videoViewNavBar' }) <$> f (_videoViewNavBar videoView')

videoViewDisconnectBaby :: Lens' (VideoView t) (Event t DeviceId)
videoViewDisconnectBaby f videoView' = (\videoViewDisconnectBaby' -> videoView' { _videoViewDisconnectBaby = videoViewDisconnectBaby' }) <$> f (_videoViewDisconnectBaby videoView')

videoViewDisconnectAll :: Lens' (VideoView t) (Event t ())
videoViewDisconnectAll f videoView' = (\videoViewDisconnectAll' -> videoView' { _videoViewDisconnectAll = videoViewDisconnectAll' }) <$> f (_videoViewDisconnectAll videoView')


