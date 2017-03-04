{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.WebRTC.Channel where

import Gonimo.Client.Prelude

import           Control.Lens
import           Control.Exception
import           Control.Monad.Reader.Class
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import qualified GHCJS.DOM.EventTarget        as ET
import           GHCJS.DOM.RTCIceCandidate
import           GHCJS.DOM.RTCIceCandidateEvent as IceEvent
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.MediaStream        as MediaStream
import           GHCJS.DOM.MediaStreamTrack
import           GHCJS.DOM.RTCPeerConnection  hiding (newRTCPeerConnection)
import qualified GHCJS.DOM.RTCPeerConnection  as RTCPeerConnection
import           Gonimo.Db.Entities           (DeviceId)
import qualified Gonimo.SocketAPI             as API
import qualified Gonimo.SocketAPI.Types       as API
import           Gonimo.Types                 (Secret)
import           Reflex.Dom

import           GHCJS.DOM.Types              (Dictionary (..),
                                               EventListener (..), MediaStream,
                                               MonadJSM)
import qualified GHCJS.DOM.Types  as ET (Event)
import           Gonimo.Client.Config
import           Gonimo.Client.Util
import           Gonimo.DOM.Window            (newRTCPeerConnection)

import           Gonimo.Client.WebRTC.Message
import           Language.Javascript.JSaddle  (function, liftJSM, (<#))
import qualified Language.Javascript.JSaddle  as JS
import           Safe                         (fromJustNote)
import           Debug.Trace (trace)
import           GHCJS.DOM.EventM as EventM
import           Data.Maybe
import           Unsafe.Coerce (unsafeCoerce)

import           Control.Concurrent (threadDelay)

data CloseEvent = CloseRequested | CloseConnectionLoss
type Message = API.Message

type Channels t = Map (API.FromId, Secret) (Channel t)
type ChannelsBehavior t = Behavior t (Channels t)

data ChannelEvent = ChannelEvent (API.FromId, Secret) RTCEvent
data RTCEvent
  = RTCEventGotRemoteStream !MediaStream
  | RTCEventNegotiationNeeded
  | RTCEventIceCandidate !RTCIceCandidate
  | RTCEventConnectionClosed

data ChannelSelector = AllChannels | OnlyChannel (API.FromId, Secret)

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configTriggerChannelEvent :: ChannelEvent -> IO ()
            , _configSourceStream :: Maybe MediaStream
            , _configTheirId :: DeviceId
            , _configSecret :: Secret
            }

data Channel t
  = Channel { _rtcConnection :: RTCPeerConnection
            , _ourStream :: Maybe MediaStream
            , _closeRequested :: Bool -- Signal that a close is requested (don't trigger alarm when connection gets closed)
            }


makeLenses ''Config
makeLenses ''Channel

channel :: forall m t. (MonadJSM m, Reflex t) => Config t -> m (Channel t)
channel config = mdo
  conn <- makeGonimoRTCConnection
  ourStream' <- runMaybeT $ do
    origStream <- MaybeT . pure $ config^.configSourceStream
    tracks <- catMaybes <$> MediaStream.getTracks origStream
    closeListener <- liftJSM . newListener $ safeClose conn
    let addCloseListener (event', track) = liftJSM $ addListener track event' closeListener False
    traverse_ addCloseListener $ (,) <$> [mute, ended] <*> tracks
    boostMediaStreamVolume origStream

  getRTCClosedEvent config conn
  handleReceivedStream config conn
  handleIceCandidate config conn
  handleNegotiationNeeded config conn
  -- let isBabyStation = isJust ourStream'
  -- alarm' <- if isBabyStation
  --           then  Just <$> loadSound "/sounds/pup_alert.mp3"
  --           else pure Nothing
  pure $ Channel { _rtcConnection = conn
                 , _ourStream = ourStream'
                 , _closeRequested = False
                 }

getRemoteStreams :: (MonadHold t m, MonadFix m, Reflex t)
                    => Event t ChannelEvent -> m (Dynamic t (Map (API.FromId, Secret) MediaStream))
getRemoteStreams ev = mdo
  let
    newStream = push (\ev' -> do
                         cStreams <- sample $ current streams
                         case ev' of
                           ChannelEvent mapKey (RTCEventGotRemoteStream stream)
                             -> pure . Just $ cStreams & at mapKey .~ Just stream
                           ChannelEvent mapKey RTCEventConnectionClosed
                             -> pure . Just $ cStreams & at mapKey .~ Nothing
                           _ -> pure Nothing
                     ) ev
  streams <- holdDyn mempty newStream
  pure streams

-- Get channels that should be closed because the RTCConnection closed or remote send close request or user pressed closed.
getClosedChannels :: Reflex t => ChannelsBehavior t -> Event t API.ServerResponse -> Event t ChannelEvent -> Event t ChannelSelector -> Event t [((API.FromId, Secret), Channel t -> Maybe (Channel t))]
getClosedChannels chans response chanEv userClose =
  let
    remoteClosed = push (\res ->
                              case res of
                                API.EventMessageReceived fromId secret' API.MsgCloseConnection
                                  -> pure . Just $ [((fromId, secret'), Just . (closeRequested .~ True))]
                                _ -> pure Nothing
                        ) response
    connClosed = push (\ev -> do
                          case ev of
                            ChannelEvent mapKey RTCEventConnectionClosed
                              -> pure . Just . trace "Channel got deleted" $ [(mapKey, const Nothing)]
                            _ -> pure Nothing
                      ) chanEv
    localClosed = push (\ev -> case ev of
                         AllChannels -> Just . map ((, Just . (closeRequested .~ True))) . Map.keys <$> sample chans
                         OnlyChannel key -> pure . Just $ [(key, Just . (closeRequested .~ True))]
                       ) userClose
  in
    remoteClosed <> connClosed <> localClosed


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

-- handles RTCPeerConnection events and produces messages to be sent to the other party.
handleRTCEvents :: ( PerformEvent t m, Reflex t, MonadSample t (Performable m)
                   , MonadJSM (Performable m), MonadIO (Performable m)
                   ) => Behavior t API.AuthData -> ChannelsBehavior t -> Event t ChannelEvent -> m (Event t [API.ServerRequest])
handleRTCEvents authData chans chanEv = do
  let
    actions = fmap (\(ChannelEvent key@(toId, secret') rtcEv) -> do
                       cChans <- sample chans
                       runMaybeT $ do
                         conn <- MaybeT . pure $ cChans^?at key._Just.rtcConnection
                         msg <- case rtcEv of
                                    RTCEventNegotiationNeeded -> liftJSM $ do
                                      jsOffer <- createOffer conn Nothing
                                      setLocalDescription conn jsOffer
                                      offer <- fromJSSessionDescription jsOffer
                                      pure $ API.MsgSessionDescriptionOffer offer
                                    RTCEventIceCandidate candidate
                                      -> liftJSM $ do
                                      API.MsgIceCandidate <$> fromJSIceCandidate candidate
                                    _            -> mzero
                         cAuthData <- lift $ sample authData
                         pure $ [API.ReqSendMessage (API.deviceId cAuthData) toId secret' msg]
                   ) chanEv
  push (pure . id) <$> performEvent actions

-- Send close messages to all channels
sendCloseMessages :: forall t. Reflex t => Behavior t API.AuthData -> ChannelsBehavior t -> Event t ChannelSelector -> Event t [API.ServerRequest]
sendCloseMessages authData chans
  = push (\ev -> do
             cAuthData <- sample authData
             let mkRequest (theirId, secret')
                           = API.ReqSendMessage (API.deviceId cAuthData) theirId secret' API.MsgCloseConnection
             case ev of
               AllChannels -> do
                 cChans <- Map.keys <$> sample chans
                 pure . Just $ map mkRequest cChans
               OnlyChannel key -> pure . Just $  [mkRequest key]
         )

handleMessages :: forall m t. ( PerformEvent t m, Reflex t, MonadSample t (Performable m)
                              , MonadJSM (Performable m), MonadIO (Performable m)
                              )
                  => Behavior t API.AuthData -> ChannelsBehavior t -> Event t API.ServerResponse
                  -> m (Event t [API.ServerRequest])
handleMessages authData chans response' = do
  let
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
                        let ourStream' = chan^.ourStream

                        resMesg <- case msg of
                          API.MsgStartStreaming -> do
                            traverse_ (addStream connection . Just) ourStream'
                            mzero
                          API.MsgSessionDescriptionOffer description -> do
                            setRemoteDescription connection =<< toJSSessionDescription description
                            rawAnswer <- liftJSM $ createAnswer connection Nothing
                            liftJSM $ setLocalDescription connection rawAnswer
                            -- setDesc <- liftJSM $ JS.eval ("(function(conn,desc) {conn.setLocalDescription(desc);})" :: Text)
                            -- liftJSM $ JS.call setDesc JS.obj (connection, rawAnswer)

                            answer <- fromJSSessionDescription rawAnswer
                            pure $ API.MsgSessionDescriptionAnswer answer
                          API.MsgSessionDescriptionAnswer description -> do
                            setRemoteDescription connection =<< toJSSessionDescription description
                            -- rawDescription <- toJSSessionDescription description
                            -- setDesc <- liftJSM $ JS.eval ("(function(conn,desc) {conn.setRemoteDescription(desc);})" :: Text)
                            -- liftJSM $ JS.call setDesc JS.obj (connection, rawDescription)
                            mzero
                          API.MsgIceCandidate candidate -> do
                            addIceCandidate connection =<< toJSIceCandidate candidate
                            mzero
                          API.MsgCloseConnection  -> do
                            safeClose connection
                            mzero
                        cAuthData <- lift $ sample authData
                        pure $ [API.ReqSendMessage (API.deviceId cAuthData) fromId secret' resMesg]
                    ) response'


  push (pure . id) <$> performEvent actions


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
                         pure . Just $ traverse_ safeClose connections
                     ) delayClose
  performEvent_ doClose


-- Handle RTCPeerConnection close.
getRTCClosedEvent :: forall m t. (MonadJSM m) => Config t -> RTCPeerConnection -> m ()
getRTCClosedEvent config conn = liftJSM $ do
  let
    triggerCloseEv = config^.configTriggerChannelEvent
                     $ ChannelEvent (config^.configTheirId, config^.configSecret) RTCEventConnectionClosed
  listener <- newListener . liftJSM $ do
    state :: Text <- getIceConnectionState conn
    if state == "closed"
      then triggerCloseEv
      else pure ()
  addListener conn iceConnectionStateChange listener False

-- Handle receiption of a remote stream (trigger channel event)
handleReceivedStream :: forall m t. (MonadJSM m)
                        => Config t -> RTCPeerConnection -> m ()
handleReceivedStream config conn = liftJSM $ do
  let triggerRTCEvent = (config^.configTriggerChannelEvent)
                        . ChannelEvent (config^.configTheirId, config^.configSecret)
                        . RTCEventGotRemoteStream
  listener <- newListener $ do
    e <- ask
    rawStream <- liftJSM $ (JS.toJSVal e) JS.! ("stream" :: Text)
    mStream <- liftJSM $ JS.fromJSVal rawStream
    liftJSM . triggerRTCEvent $ fromJustNote "event had no valid MediaStream!" mStream

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
    mCandidate <- IceEvent.getCandidate e
    liftIO $ traverse_ triggerRTCEvent mCandidate
  addListener conn iceCandidate listener False

makeGonimoRTCConnection :: MonadJSM m => m RTCPeerConnection
makeGonimoRTCConnection = liftJSM $ do
  config <- JS.obj
  config <# ("urls" :: Text) $ JS.toJSVal [gonimoTurnServer]
  config <# ("username" :: Text)$ JS.toJSVal gonimoTurnUser
  config <# ("credential" :: Text) $ JS.toJSVal gonimoTurnPassword
  config <# ("credentialType" :: Text) $ JS.toJSVal gonimoTurnCredentialType
  let configDic = case config of JS.Object val -> Dictionary val
  newRTCPeerConnection $ Just configDic

-- Don't use plain close, it throws uncatchable exceptions when connection is already closed:
safeClose :: MonadJSM m => RTCPeerConnection -> m ()
safeClose conn = liftJSM $ do
      jsClose <- JS.eval $ ("(function(conn) { try {conn.close();} catch(e) {console.log(\"Catched: \" + e.toString());}})" :: Text)
      _ <- JS.call jsClose JS.obj [conn]
      pure ()
