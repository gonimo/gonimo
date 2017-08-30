{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Gonimo.Client.WebRTC.Channels where

import Gonimo.Client.Prelude

import           Control.Concurrent
import           Control.Lens
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
-- Workaround until issue https://github.com/ghcjs/jsaddle-dom/issues/3 is resolved:
#ifdef __GHCJS__
import           GHCJS.DOM.RTCPeerConnection   as RTCPeerConnection
#else
-- import           JSDOM.Custom.RTCPeerConnection  as RTCPeerConnection hiding (newRTCPeerConnection)
import           JSDOM.Generated.RTCPeerConnection  as RTCPeerConnection
#endif
import           Gonimo.Db.Entities            (DeviceId)
import qualified Gonimo.SocketAPI              as API
import qualified Gonimo.SocketAPI.Types        as API
import           Gonimo.Types                  (Secret)
import           Reflex.Dom.Core

import           GHCJS.DOM.Types               (MediaStream, MediaStreamTrack,
                                                MonadJSM, RTCIceCandidate(..), RTCIceCandidateInit(..))
import qualified GHCJS.DOM.Types               as JS

import           Data.Maybe
import           Debug.Trace                   (trace)
import qualified Gonimo.SocketAPI.Translations as API
import           Language.Javascript.JSaddle   (JSM, liftJSM)
import qualified Language.Javascript.JSaddle   as JS

import           GHCJS.DOM.EventM              (on)
import qualified GHCJS.DOM.MediaStream         as MediaStream
import           GHCJS.DOM.MediaStreamTrack    (getMuted, mute, unmute)
import           Gonimo.Client.Reflex          (buildDynMap)
import           Gonimo.Client.Util            (getTransmissionInfo, showJSException, fromPromiseM)
import           Gonimo.Client.WebRTC.Channel  (Channel (..), ChannelEvent (..),
                                                CloseEvent (..), RTCEvent (..),
                                                ReceivingState (..),
                                                audioReceivingState,
                                                closeRequested, rtcConnection,
                                                theirStream,
                                                videoReceivingState)
import qualified Gonimo.Client.WebRTC.Channel  as Channel
import qualified Data.Text.IO as T

type ChannelMap t = Map (API.FromId, Secret) (Channel.Channel t)
type StreamMap = Map (API.FromId, Secret) MediaStream
type ChannelsBehavior t = Behavior t (ChannelMap t)

data ChannelSelector = AllChannels | OnlyChannel (DeviceId, Secret)

-- Channels encapsulate RTCPeerConnections and combine them with a signalling channel.
data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configOurId :: Dynamic t DeviceId
            , _configBroadcastStream :: Dynamic t (Maybe MediaStream)
            , _configCreateChannel :: Event t (DeviceId, Secret)
            , _configCloseChannel :: Event t ChannelSelector
            }

data Channels t
  = Channels { _channelMap :: Dynamic t (ChannelMap t)
             , _request :: Event t [ API.ServerRequest ]
             , _remoteStreams :: Dynamic t StreamMap -- Useful to have this separate for rendering. (Don't reload videos on every change to map.)
             }



makeLenses ''Config
makeLenses ''Channels


channels :: forall m t. GonimoM t m => Config t -> m (Channels t)
channels config = mdo
  (channelEvent, triggerChannelEvent) <- newTriggerEvent
  insertChannel <- handleCreateChannel config triggerChannelEvent
  (removeChannel, sendCloseRequest) <- handleCloseChannel config channels' channelEvent
  let streamUpdates = handleRemoteStreams channelEvent
  let updateStreamChannels = fst <$> streamUpdates
  let updateStreams = snd <$> streamUpdates
  updateReceivingState <- handleConnectionStateUpdate (current channels') channelEvent
  updateMuteState <- handleMuteUpdate (current channels') channelEvent

  channels' <- holdDyn Map.empty . buildDynMap (current channels') $ [insertChannel, removeChannel, updateStreamChannels] <> updateReceivingState <> updateMuteState

  rtcRequest <- handleRTCEvents (current $ config^.configOurId) (current channels') channelEvent

  handleBroadcastStream config channels'
  msgRequest <- handleMessages config (current channels')

  rStreams <- holdDyn Map.empty . buildDynMap (current rStreams) $ [updateStreams]

  pure $ Channels { _request = sendCloseRequest <> msgRequest <> rtcRequest
                  , _channelMap = channels'
                  , _remoteStreams = rStreams
                  }

-- Get notified when a channel gets closed.
getCloseEvent :: Reflex t => ChannelsBehavior t -> Event t ChannelEvent -> Event t ((API.FromId,Secret), CloseEvent)
getCloseEvent chans
  = push (\ev' -> do
             cChans <- sample chans
             case ev' of
               ChannelEvent mapKey RTCEventConnectionClosed
                            -> if cChans^? at mapKey . _Just.closeRequested /= Just True
                                  then pure . Just $ (mapKey, CloseConnectionLoss)
                                  else pure . Just $ (mapKey, CloseRequested)
               _            -> pure Nothing
         )

handleMuteUpdate :: forall t m. ( TriggerEvent t m, MonadJSM m, Reflex t, PerformEvent t m
                                , MonadJSM (Performable m)
                                )
                    => ChannelsBehavior t -> Event t ChannelEvent
                    -> m [Event t (ChannelMap t -> ChannelMap t)]
handleMuteUpdate chans chanEv = do
    (statUpdate, triggerStatUpdate) <- newTriggerEvent
    let
      newStreamEv = newStreamEvent chans chanEv
    let
      handleMute :: forall m1. (MonadIO m1, MonadJSM m1)
                    =>  ((DeviceId, Secret), MediaStream, RTCPeerConnection) -> m1 ()
      handleMute (mapKey, stream, _) = liftJSM $ do
        videoTracks <- MediaStream.getVideoTracks stream
        audioTracks <- MediaStream.getAudioTracks stream
        let
          registerMuteHandlers :: Lens' (Channel t) Bool -> MediaStreamTrack -> JSM ()
          registerMuteHandlers audioVideo track' = do
              isMuted <- getMuted track'
              liftIO $ triggerStatUpdate (at mapKey._Just.audioVideo .~ isMuted)
              _ <- on track' mute $ do
                liftIO $ threadDelay 3500000
                stillMuted <- getMuted track'
                liftIO . when stillMuted $
                  triggerStatUpdate (at mapKey._Just.audioVideo .~ True)
              _ <- on track' unmute . liftIO $ triggerStatUpdate (at mapKey._Just.audioVideo .~ False)
              pure ()
        traverse_ (registerMuteHandlers Channel.audioMuted) audioTracks
        traverse_ (registerMuteHandlers Channel.videoMuted) videoTracks

    performEvent_ $ handleMute <$> newStreamEv
    pure [statUpdate]

handleConnectionStateUpdate :: forall t m. ( TriggerEvent t m, MonadJSM m, Reflex t, PerformEvent t m
                                           , MonadJSM (Performable m)
                                           )
                               => ChannelsBehavior t -> Event t ChannelEvent
                            -> m [Event t (ChannelMap t -> ChannelMap t)]
handleConnectionStateUpdate chans chanEv = do
    (statUpdate, triggerStatUpdate) <- newTriggerEvent
    let
      newStreamEv = newStreamEvent chans chanEv
    let
      updateState :: Traversal' (ChannelMap t) ReceivingState -> Int -> JSM ()
      updateState stateField newStat = liftIO $ triggerStatUpdate (stateField %~ updateStat newStat)

    let
      registerGetTransmissionInfo (mapKey, stream, conn) = do
        videoTracks <- MediaStream.getVideoTracks stream
        audioTracks <- MediaStream.getAudioTracks stream

        let
          sendStats :: forall m1. (MonadJSM m1)
                       => Lens' (Channel t) ReceivingState -> MediaStreamTrack -> m1 ()
          sendStats audioVideo = getTransmissionInfo conn $ updateState (at mapKey._Just.audioVideo)

        traverse_ (sendStats audioReceivingState) audioTracks
        traverse_ (sendStats videoReceivingState) videoTracks

        pure $ at mapKey . _Just %~ over audioReceivingState (makeUnreliable (not . null $ audioTracks))
                                  . over videoReceivingState (makeUnreliable (not . null $ videoTracks))


    setUnreliable' <- performEvent $ registerGetTransmissionInfo <$> newStreamEv
    setUnreliable <- delay 4 setUnreliable'
    pure [setUnreliable, statUpdate]

  where
    makeUnreliable :: Bool -> ReceivingState -> ReceivingState
    makeUnreliable True oldState = case oldState of
                                     StateNotReceiving -> StateUnreliable
                                     _                 -> oldState
    makeUnreliable False oldState = oldState

    updateStat :: Int -> ReceivingState -> ReceivingState
    updateStat mStat oldState = case (mStat, oldState) of
      (0, StateNotReceiving) -> oldState
      (_, StateNotReceiving)  -> StateReceiving 0
      (0, StateUnreliable)   -> oldState
      (_, StateUnreliable)   -> StateReceiving 0
      (0, StateReceiving n)  -> StateReceiving (n+1)
      (_, StateReceiving 0)  -> oldState
      (_, StateReceiving n)  -> StateReceiving (n-1) -- We can go back to  a reliable state, but we have to build up some trust again.

-- Little helper frunction for handleConnectionStateUpdate and handleMuteUpdate
newStreamEvent :: Reflex t => ChannelsBehavior t -> Event t ChannelEvent -> Event t ((DeviceId, Secret), MediaStream, RTCPeerConnection)
newStreamEvent chans = push (\ev' ->
                      case ev' of
                        ChannelEvent mapKey (RTCEventGotRemoteStream stream) -> runMaybeT $ do
                          cChans <- lift $ sample chans
                          conn <- MaybeT . pure $ cChans^?at mapKey._Just.rtcConnection
                          pure (mapKey, stream, conn)
                        _ -> pure Nothing
                    )

handleRemoteStreams :: Reflex t
                    => Event t ChannelEvent -> Event t (ChannelMap t -> ChannelMap t, StreamMap -> StreamMap)
handleRemoteStreams
    = push (\ev' -> do
              case ev' of
                ChannelEvent mapKey (RTCEventGotRemoteStream stream)
                  -> pure . Just $ (at mapKey . _Just . theirStream .~ Just stream, at mapKey .~ Just stream)
                ChannelEvent mapKey RTCEventRemoteStreamEnded
                  -> pure . Just $ (at mapKey . _Just . theirStream .~ Nothing, at mapKey .~ Nothing)
                _ -> pure Nothing
           )

handleBroadcastStream :: forall t m. ( MonadHold t m, MonadFix m, Reflex t, Applicative (Performable m)
                                     , MonadJSM (Performable m), PerformEvent t m
                                     )
                    => Config t -> Dynamic t (ChannelMap t) -> m ()
handleBroadcastStream config channels' = do
    let
      initializeNewChans :: ChannelMap t -> PushM t (Maybe (Performable m ()))
      initializeNewChans upChans = runMaybeT $ do
        oldChans <- lift . sample . current $ channels'
        let newChans = Map.elems $ Map.difference upChans oldChans
        let connections = (^.rtcConnection) <$> newChans
        cStream <- MaybeT . sample . current $ config^.configBroadcastStream
        pure $ traverse_ (flip addStream cStream) connections

    performEvent_ $ push initializeNewChans (updated channels')

    let
      replaceStreams :: Maybe MediaStream -> PushM t (Performable m ())
      replaceStreams mNewStream = do
        cChans <- sample . current $ channels'
        let connections = (^.rtcConnection) <$> Map.elems cChans
        mOldStream <- sample . current $ config^.configBroadcastStream
        let oldStreams = maybeToList mOldStream
        let newStreams = maybeToList mNewStream
        pure $ do
          sequence_ $ removeStream <$> connections <*> oldStreams
          sequence_ $ addStream <$> connections <*> newStreams
      
    performEvent_ $ pushAlways replaceStreams (updated $ config^.configBroadcastStream)

handleCreateChannel :: ( MonadHold t m, MonadFix m, Reflex t, PerformEvent t m
                       , MonadJSM (Performable m)
                       )
                    => Config t -> (ChannelEvent -> IO ()) -> m (Event t (ChannelMap t -> ChannelMap t))
handleCreateChannel config triggerChannelEvent
  = performEvent $ buildChannel <$> config^.configCreateChannel
  where
    buildChannel key@(deviceId, secret) = do
      chan <- Channel.channel $ Channel.Config { Channel._configResponse = config^.configResponse
                                               , Channel._configTriggerChannelEvent = triggerChannelEvent
                                               , Channel._configTheirId = deviceId
                                               , Channel._configSecret = secret
                                               }
      pure (at key .~ Just chan)

handleCloseChannel :: ( MonadHold t m, MonadFix m, Reflex t, PerformEvent t m
                      , TriggerEvent t m, MonadJSM m, MonadJSM (Performable m)
                      )
                    => Config t -> Dynamic t (ChannelMap t) -> Event t ChannelEvent -> m (Event t (ChannelMap t -> ChannelMap t), Event t [API.ServerRequest])
handleCloseChannel config channels' channelEvent = do
    closeRTCConnections (current $ channels') (config^.configCloseChannel)
    let closeRequests = sendCloseMessages (current $ config^.configOurId) (current channels') (config^.configCloseChannel)
    let removeChan = getClosedChannels (config^.configResponse) channelEvent (config^.configCloseChannel)
    pure (removeChan, closeRequests)


-- Close all rtc connections with a delay so a close message can be sent first. (Avoid alarm.)
closeRTCConnections :: forall m t. ( MonadJSM m, MonadIO m, PerformEvent t m
                                   , TriggerEvent t m, MonadJSM (Performable m)
                                   , MonadIO (Performable m)
                                   )
                       => ChannelsBehavior t -> Event t ChannelSelector -> m ()
closeRTCConnections chans userClose = do
  -- delayClose <- delay 1 userClose
  let delayClose = userClose -- Let's try without delay for now.
  let doClose = push (\ev -> do
                         cChansMap <- sample chans
                         let cChans = Map.toList cChansMap
                         let connections = case ev of
                                             AllChannels -> over mapped (^._2.rtcConnection) cChans
                                             OnlyChannel key -> maybeToList $ cChansMap^?at key._Just.rtcConnection
                         pure . Just $ do
                           traverse_ RTCPeerConnection.close connections
                           traverse_ RTCPeerConnection.close connections
                     ) delayClose
  performEvent_ doClose

-- Send close messages to all channels
sendCloseMessages :: forall t. Reflex t => Behavior t DeviceId -> ChannelsBehavior t -> Event t ChannelSelector -> Event t [API.ServerRequest]
sendCloseMessages ourId chans
  = push (\ev -> do
             cOurId <- sample ourId
             let mkRequest (theirId, secret')
                           = API.ReqSendMessage cOurId theirId secret' API.MsgCloseConnection
             case ev of
               AllChannels -> do
                 cChans <- Map.keys <$> sample chans
                 pure . Just $ map mkRequest cChans
               OnlyChannel key -> pure . Just $  [mkRequest key]
         )

-- Get channels that should be closed because the RTCConnection closed or remote send close request or user pressed closed.
getClosedChannels :: Reflex t => Event t API.ServerResponse -> Event t ChannelEvent -> Event t ChannelSelector -> Event t (ChannelMap t -> ChannelMap t)
getClosedChannels response chanEv userClose =
  let
    remoteClosed = push (\res ->
                              case res of
                                API.EventMessageReceived fromId secret' API.MsgCloseConnection
                                  -> pure . Just $ (at (fromId, secret') . _Just . Channel.closeRequested .~ True)
                                _ -> pure Nothing
                        ) response
    connClosed = push (\ev -> do
                          case ev of
                            ChannelEvent mapKey RTCEventConnectionClosed
                              -> pure . Just . trace "Channel got deleted" $ (at mapKey .~ Nothing)
                            _ -> pure Nothing
                      ) chanEv
    localClosed = push (\ev -> case ev of
                         AllChannels -> pure . Just $ (over mapped (closeRequested .~ True))
                         OnlyChannel key -> pure . Just $ (at key . _Just . Channel.closeRequested .~ True)
                       ) userClose
  in
    remoteClosed <> connClosed <> localClosed

handleMessages :: forall m t. ( PerformEvent t m, Reflex t, MonadSample t (Performable m)
                              , MonadJSM (Performable m), MonadIO (Performable m)
                              )
                  => Config t -> ChannelsBehavior t -> m (Event t [API.ServerRequest])
handleMessages config chans = do
  let
      response' = config^.configResponse

      getMessage :: MonadPlus f => API.ServerResponse -> f (API.FromId, Secret, API.Message)
      getMessage res = case res of
                         API.EventMessageReceived fromId secret' msg -> do
                           pure (fromId, secret', msg)
                         _ -> mzero

      actions :: forall m1. (MonadJSM m1, MonadSample t m1) => Event t (m1 (Maybe [API.ServerRequest]))
      actions = fmap (\res -> runMaybeT $ do
                        (fromId, secret', msg) <- getMessage res
                        cChans <- lift $ sample chans
                        chan <- MaybeT . pure $ cChans^.at (fromId, secret')
                        let connection = chan^.rtcConnection

                        resMesg <- handleRejectedWithClose connection
                          $ case msg of
                              API.MsgSessionDescriptionOffer description -> do
                                setRemoteDescription connection =<< API.mToFrontend description
                                rawAnswer <- createAnswer connection Nothing
                                liftJSM $ setLocalDescription connection rawAnswer
                                -- setDesc <- liftJSM $ JS.eval ("(function(conn,desc) {conn.setLocalDescription(desc);})" :: Text)
                                -- liftJSM $ JS.call setDesc JS.obj (connection, rawAnswer)

                                answer <- API.mFromFrontend rawAnswer
                                pure $ API.MsgSessionDescriptionAnswer answer
                              API.MsgSessionDescriptionAnswer description -> do
                                setRemoteDescription connection =<< API.mToFrontend description
                                -- rawDescription <- toJSSessionDescription description
                                -- setDesc <- liftJSM $ JS.eval ("(function(conn,desc) {conn.setRemoteDescription(desc);})" :: Text)
                                -- liftJSM $ JS.call setDesc JS.obj (connection, rawDescription)
                                mzero
                              API.MsgIceCandidate candidate -> do
                                addIceCandidate connection =<< API.mToFrontend candidate
                                mzero
                              API.MsgCloseConnection  -> do
                                RTCPeerConnection.close connection
                                RTCPeerConnection.close connection
                                mzero
                        ourId' <- lift $ sample (current $ config^.configOurId)
                        pure $ [API.ReqSendMessage ourId' fromId secret' resMesg]
                    ) response'


  push (pure . id) <$> performEvent actions

-- handles RTCPeerConnection events and produces messages to be sent to the other party.
handleRTCEvents :: forall t m. ( PerformEvent t m, Reflex t, MonadSample t (Performable m)
                   , MonadJSM (Performable m), MonadIO (Performable m)
                   ) => Behavior t DeviceId -> ChannelsBehavior t -> Event t ChannelEvent -> m (Event t [API.ServerRequest])
handleRTCEvents ourId chans chanEv = do
  let
    actions :: Event t (Performable m (Maybe [API.ServerRequest]))
    actions = fmap (\(ChannelEvent key@(toId, secret') rtcEv) -> do
                       cChans <- sample chans
                       cOurId <- sample ourId

                       runMaybeT $ do
                         conn <- MaybeT . pure $ cChans^?at key._Just.rtcConnection

                         msg <- handleRejectedWithClose conn
                           $ case rtcEv of
                                RTCEventNegotiationNeeded -> do
                                  jsOffer <- createOffer conn Nothing
                                  setLocalDescription conn jsOffer
                                  offer <- API.mFromFrontend jsOffer
                                  pure $ API.MsgSessionDescriptionOffer offer
                                RTCEventIceCandidate candidate
                                  -> API.MsgIceCandidate <$> API.mFromFrontend (rtcIceCandidateToInit candidate)
                                _            -> mzero
                         pure $ [API.ReqSendMessage cOurId toId secret' msg]
                   ) chanEv
  push (pure . id) <$> performEvent actions


-- | Specialized promise handling for `handleRTCEvents` and `handleMessages`
handleRejectedWithClose :: forall m. MonadJSM m => RTCPeerConnection -> MaybeT JSM API.Message -> MaybeT m API.Message
handleRejectedWithClose conn = MaybeT . fromPromiseM onError . runMaybeT
  where
    onError = do
      RTCPeerConnection.close conn
      pure $ Just API.MsgCloseConnection


rtcIceCandidateToInit :: RTCIceCandidate -> RTCIceCandidateInit
rtcIceCandidateToInit (RTCIceCandidate cand) = RTCIceCandidateInit cand


-- handlePromiseRejectedT :: forall a. JSM a -> MaybeT JSM a
-- handlePromiseRejectedT = MaybeT . handlePromiseRejected

-- handlePromiseRejected :: forall a. JSM a -> JSM (Maybe a)
-- handlePromiseRejected action = fmap Just action `JS.catch` handleException
--   where
--     -- TODO: Properly print JS exception:
--     handleException :: JS.PromiseRejected -> JSM (Maybe a)
--     handleException e = do
--       putStrLn "Handled rejected promise"
--       pure Nothing
