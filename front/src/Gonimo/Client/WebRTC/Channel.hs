{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Gonimo.Client.WebRTC.Channel where

import           Gonimo.Client.Prelude



import           GHCJS.DOM.Enums                (MediaStreamTrackState (..))
import           GHCJS.DOM.EventM
import           GHCJS.DOM.RTCIceCandidate
import           GHCJS.DOM.RTCIceCandidateEvent as IceEvent
-- #ifdef __GHCJS__
import           GHCJS.DOM.RTCPeerConnection    as RTCPeerConnection
-- #else
-- import           JSDOM.Custom.RTCPeerConnection  as RTCPeerConnection hiding (newRTCPeerConnection)
-- -- import           JSDOM.Generated.RTCPeerConnection  as RTCPeerConnection (addStream)
-- #endif
import           GHCJS.DOM.Types                (Dictionary (..), MediaStream,
                                                 MonadJSM, RTCPeerConnection)
import           Gonimo.Client.Environment      (HasEnvironment)
import qualified Gonimo.Client.Environment      as Env
import           Gonimo.DOM.Window              (newRTCPeerConnection)
import qualified Gonimo.SocketAPI               as API
import           Gonimo.SocketAPI.Types         (DeviceId)
import qualified Gonimo.SocketAPI.Types         as API
import           Gonimo.Types                   (Secret)


import           GHCJS.DOM.Enums                (RTCIceConnectionState (..))
import qualified GHCJS.DOM.MediaStream          as MediaStream
import           GHCJS.DOM.MediaStreamTrack     (ended, getReadyState)
import           Language.Javascript.JSaddle    ((<#))
import qualified Language.Javascript.JSaddle as JS

import           Safe                           (fromJustNote)

data CloseEvent = CloseRequested | CloseConnectionLoss
type Message = API.Message

data ChannelEvent = ChannelEvent (API.FromId, Secret) RTCEvent
data RTCEvent
  = RTCEventGotRemoteStream !MediaStream
  | RTCEventNegotiationNeeded
  | RTCEventIceCandidate !RTCIceCandidate
  | RTCEventConnectionClosed
  | RTCEventRemoteStreamEnded

data ReceivingState
  = StateNotReceiving
  | StateUnreliable -- We got a stream but did not get any stats for more than a few seconds!
  | StateReceiving Int -- All is fine - we are receiving stats, contains number of uninterrupted zero package count updates.
  deriving (Eq, Ord, Show)

-- If StateReceiving counter is equal or above this number a connection can be considered broken (alarm should be triggered)
considerBrokenCount :: Int
considerBrokenCount = 4

isStateBroken :: ReceivingState -> Bool
isStateBroken (StateReceiving n) = n >= considerBrokenCount
isStateBroken _                  = False

data Config t
  = Config  { _configResponse            :: Event t API.ServerResponse
            , _configTriggerChannelEvent :: ChannelEvent -> IO ()
            , _configTheirId             :: DeviceId
            , _configSecret              :: Secret
            }

data Channel t
  = Channel { _rtcConnection       :: RTCPeerConnection
            , _theirStream         :: Maybe MediaStream
            , _closeRequested      :: Bool -- Signal that a close is requested (don't trigger alarm when connection gets closed)
            , _audioReceivingState :: ReceivingState
            , _videoReceivingState :: ReceivingState
            , _audioMuted          :: Bool
            , _videoMuted          :: Bool
            }

channel :: forall model m t. (HasEnvironment model, MonadJSM m, Reflex t) => model t -> Config t -> m (Channel t)
channel model config = mdo
  conn <- makeGonimoRTCConnection model

  handleRTCClosedEvent config conn
  handleReceivedStream config conn
  handleIceCandidate config conn
  handleNegotiationNeeded config conn

  pure $ Channel { _rtcConnection = conn
                 , _theirStream = Nothing
                 , _closeRequested = False
                 , _audioReceivingState = StateNotReceiving
                 , _videoReceivingState = StateNotReceiving
                 , _audioMuted = False
                 , _videoMuted = False
                 }

-- Get the worst state available for a channel.
worstState :: Channel t -> ReceivingState
worstState chan = if badness (chan^.audioReceivingState) > badness (chan^.videoReceivingState)
                  then chan^.audioReceivingState
                  else chan^.videoReceivingState
  where
    badness :: ReceivingState -> Int
    badness (StateReceiving n)
      | n < considerBrokenCount = 0
      | otherwise = 3
    badness StateNotReceiving = 1
    badness StateUnreliable = 2

needsAlert :: Channel t -> Bool
needsAlert chan = isStateBroken (chan^.audioReceivingState)
                  || isStateBroken (chan^.videoReceivingState)
                  || chan^.audioMuted
                  || chan^.videoMuted

-- Handle RTCPeerConnection close.
handleRTCClosedEvent :: forall m t. (MonadJSM m) => Config t -> RTCPeerConnection -> m ()
handleRTCClosedEvent config conn = liftJSM $ do
  let
    triggerCloseEv = config^.configTriggerChannelEvent
                     $ ChannelEvent (config^.configTheirId, config^.configSecret) RTCEventConnectionClosed
  listener <- newListener $ do
    state <- liftJSM $ getIceConnectionState conn
    if state == RTCIceConnectionStateClosed
      then liftIO $ triggerCloseEv
      else pure ()
  addListener conn iceConnectionStateChange listener False

-- Handle receiption of a remote stream (trigger channel event)
handleReceivedStream :: forall m t. (MonadJSM m)
                        => Config t -> RTCPeerConnection -> m ()
handleReceivedStream config conn = liftJSM $ do
  let mapKey = (config^.configTheirId, config^.configSecret)
  let triggerChannelEvent = config^.configTriggerChannelEvent
  let triggerRTCEvent = triggerChannelEvent
                        . ChannelEvent mapKey
                        . RTCEventGotRemoteStream
  listener <- newListener $ do
    e <- ask
    rawStream <- liftJSM $ (JS.toJSVal e) JS.! ("stream" :: Text)
    mStream <- liftJSM $ JS.fromJSVal rawStream
    let stream = fromJustNote "event had no valid MediaStream!" mStream

    tracks <- MediaStream.getTracks stream
    let sendStreamEnded = do
          cStates <- traverse getReadyState tracks
          liftIO . when (all (== MediaStreamTrackStateEnded) cStates) $
            triggerChannelEvent $ ChannelEvent mapKey RTCEventRemoteStreamEnded

    endedListener <- lift . newListener $ sendStreamEnded

    let addEndedListener (event', track') = liftJSM $ addListener track' event' endedListener False
    traverse_ addEndedListener $ (ended,) <$> tracks
    sendStreamEnded

    liftIO . triggerRTCEvent $ stream

  addListener conn addStreamEvent listener False

handleNegotiationNeeded :: forall m t. (MonadJSM m)
                           => Config t -> RTCPeerConnection -> m ()
handleNegotiationNeeded config conn = liftJSM $ do
  let triggerRTCEvent = (config^.configTriggerChannelEvent)
                        . ChannelEvent (config^.configTheirId, config^.configSecret)
                        $ RTCEventNegotiationNeeded
  listener <- newListener . liftIO $ triggerRTCEvent
  addListener conn negotiationNeeded listener False

handleIceCandidate :: forall m t. (MonadJSM m)
                           => Config t -> RTCPeerConnection -> m ()
handleIceCandidate config conn = liftJSM $ do
  let triggerRTCEvent = (config^.configTriggerChannelEvent)
                        . ChannelEvent (config^.configTheirId, config^.configSecret)
                        . RTCEventIceCandidate
  listener <- newListener $ do
    e <- ask
    candidate <- IceEvent.getCandidate e
    -- Needed, until https://github.com/ghcjs/ghcjs-dom/issues/73 gets fixed:
    -- We should also check again whether we should send a null candidate to the other party.
    -- Spec says yes, as far as I remember, but implementation crashed last time.
    case candidate of
      RTCIceCandidate jsVal -> do
        candidateIsNull <- liftJSM $ JS.ghcjsPure $ JS.isNull jsVal
        unless candidateIsNull $
          liftIO $ triggerRTCEvent candidate
  addListener conn iceCandidate listener False

makeGonimoRTCConnection :: (MonadJSM m, HasEnvironment model) => model t -> m RTCPeerConnection
makeGonimoRTCConnection model = liftJSM $ do
  config <- JS.obj
  config <# ("urls" :: Text) $ JS.toJSVal [model ^. Env.turnConnection]
  config <# ("username" :: Text)$ JS.toJSVal (model ^. Env.turnUser)
  config <# ("credential" :: Text) $ JS.toJSVal (model ^. Env.turnPassword)
  config <# ("credentialType" :: Text) $ JS.toJSVal (model ^. Env.turnCredentialType)
  allServers <- JS.obj
  allServers <# ("iceServers" :: Text) $ [config]
  let configDic = case allServers of JS.Object val -> Dictionary val
  newRTCPeerConnection $ Just configDic

-- Don't use plain close, it throws uncatchable exceptions when connection is already closed:
safeClose :: MonadJSM m => RTCPeerConnection -> m ()
safeClose conn = liftJSM $ do
      jsClose <- JS.eval $ ("(function(conn) { try {conn.close();} catch(e) {console.log(\"Caught: \" + e.toString());}})" :: Text)
      _ <- JS.call jsClose JS.obj [conn]
      pure ()


-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configTriggerChannelEvent :: Lens' (Config t) (ChannelEvent -> IO ())
configTriggerChannelEvent f config' = (\configTriggerChannelEvent' -> config' { _configTriggerChannelEvent = configTriggerChannelEvent' }) <$> f (_configTriggerChannelEvent config')

configTheirId :: Lens' (Config t) DeviceId
configTheirId f config' = (\configTheirId' -> config' { _configTheirId = configTheirId' }) <$> f (_configTheirId config')

configSecret :: Lens' (Config t) Secret
configSecret f config' = (\configSecret' -> config' { _configSecret = configSecret' }) <$> f (_configSecret config')


-- Lenses for Channel t:

rtcConnection :: Lens' (Channel t) RTCPeerConnection
rtcConnection f channel' = (\rtcConnection' -> channel' { _rtcConnection = rtcConnection' }) <$> f (_rtcConnection channel')

theirStream :: Lens' (Channel t) (Maybe MediaStream)
theirStream f channel' = (\theirStream' -> channel' { _theirStream = theirStream' }) <$> f (_theirStream channel')

closeRequested :: Lens' (Channel t) Bool
closeRequested f channel' = (\closeRequested' -> channel' { _closeRequested = closeRequested' }) <$> f (_closeRequested channel')

audioReceivingState :: Lens' (Channel t) ReceivingState
audioReceivingState f channel' = (\audioReceivingState' -> channel' { _audioReceivingState = audioReceivingState' }) <$> f (_audioReceivingState channel')

videoReceivingState :: Lens' (Channel t) ReceivingState
videoReceivingState f channel' = (\videoReceivingState' -> channel' { _videoReceivingState = videoReceivingState' }) <$> f (_videoReceivingState channel')

audioMuted :: Lens' (Channel t) Bool
audioMuted f channel' = (\audioMuted' -> channel' { _audioMuted = audioMuted' }) <$> f (_audioMuted channel')

videoMuted :: Lens' (Channel t) Bool
videoMuted f channel' = (\videoMuted' -> channel' { _videoMuted = videoMuted' }) <$> f (_videoMuted channel')


