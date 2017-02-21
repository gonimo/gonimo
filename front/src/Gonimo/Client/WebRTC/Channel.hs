{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.WebRTC.Channel where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Maybe                   (catMaybes)
import           GHCJS.DOM.EventTarget
import qualified GHCJS.DOM.MediaStream        as MediaStream
import           GHCJS.DOM.RTCPeerConnection  hiding (newRTCPeerConnection)
import qualified GHCJS.DOM.RTCPeerConnection  as RTCPeerConnection
import qualified Gonimo.SocketAPI.Types       as API
import           Reflex.Dom

import           GHCJS.DOM.Types              (Dictionary (..),
                                               EventListener (..), MediaStream,
                                               MonadJSM)
import           Gonimo.Client.Config
import           Gonimo.Client.Util
import           Gonimo.DOM.Window            (newRTCPeerConnection)

import           Gonimo.Client.WebRTC.Message
import           Language.Javascript.JSaddle  (function, liftJSM, (<#))
import qualified Language.Javascript.JSaddle  as JS
import           Safe                         (fromJustNote)

data CloseEvent = CloseRequested | CloseConnectionLoss
type Message = API.Message

data Config t
  = Config  { _configGotMessage :: Event t Message
            , _configSourceStream :: Maybe MediaStream
            , _configClose :: Event t ()
            }

data Channel t
  = Channel { _sendMessage :: Event t Message
            , _rtcConnection :: RTCPeerConnection
            , _ourStream :: Maybe MediaStream
            , _remoteStream :: Dynamic t (Maybe MediaStream)
            , _closed :: Event t CloseEvent
            }


makeLenses ''Config
makeLenses ''Channel

channel :: forall m t. (MonadWidget t m)
        => Config t -> m (Channel t)
channel config = mdo
  conn <- makeGonimoRTCConnection
  ourStream' <- runMaybeT $ do
    origStream <- MaybeT . pure $ config^.configSourceStream
    tracks <- catMaybes <$> MediaStream.getTracks origStream
    let
      addListener :: forall m1 track. (MonadJSM m1, IsEventTarget track) => Text -> track -> m1 ()
      addListener event track = do
        jsFun <- liftJSM . function $ \_ _ [] -> RTCPeerConnection.close conn
        listener <- liftJSM $ EventListener <$> JS.toJSVal jsFun
        addEventListener track event (Just listener) False
    traverse_ (uncurry addListener) $ (,) <$> ["ended", "mute", "inactive"] <*> tracks -- all permutations!
    boostMediaStreamVolume origStream

  (messages, remoteClosed) <- handleMessages config conn ourStream'

  gotRemoteStream <- handleReceivedStream conn
  remoteStream' <- holdDyn Nothing $ Just <$> gotRemoteStream
  -- let isBabyStation = isJust ourStream'
  -- alarm' <- if isBabyStation
  --           then  Just <$> loadSound "/sounds/pup_alert.mp3"
  --           else pure Nothing
  pure $ Channel { _sendMessage = messages
                 , _rtcConnection = conn
                 , _ourStream = ourStream'
                 , _remoteStream = remoteStream'
                 , _closed = const CloseRequested <$> remoteClosed
                 }

handleReceivedStream :: forall m t. (MonadWidget t m) => RTCPeerConnection -> m (Event t MediaStream)
handleReceivedStream conn = do
  (rtcEvent', triggerRTCEvent) <- newTriggerEvent
  let
    addListener :: forall m1 self. (MonadJSM m1, IsEventTarget self) => Text -> self -> m1 ()
    addListener event self = do
      jsFun <- liftJSM . function $ \_ _ [mediaEvent] -> do
        rawStream <- (JS.toJSVal mediaEvent) JS.! ("stream" :: Text)
        mStream <- JS.fromJSVal rawStream
        triggerRTCEvent $ fromJustNote "event had no valid MediaStream!" mStream

      listener <- liftJSM $ EventListener <$> JS.toJSVal jsFun
      addEventListener self event (Just listener) False
  addListener "addstream" conn
  pure rtcEvent'

handleMessages :: forall m t. (MonadWidget t m) => Config t -> RTCPeerConnection -> Maybe MediaStream -> m (Event t API.Message, Event t ())
handleMessages config connection ourStream' = do
  let actions = fmap (\msg ->
                        case msg of
                         API.MsgStartStreaming -> do
                           traverse_ (addStream connection . Just) ourStream'
                           pure Nothing
                         API.MsgSessionDescriptionOffer description -> do
                           setRemoteDescription connection =<< toJSSessionDescription description
                           rawAnswer <- createAnswer connection Nothing
                           setLocalDescription connection rawAnswer
                           answer <- fromJSSessionDescription rawAnswer
                           pure . Just $ API.MsgSessionDescriptionAnswer answer
                         API.MsgSessionDescriptionAnswer description -> do
                           setRemoteDescription connection =<< toJSSessionDescription description
                           pure Nothing
                         API.MsgIceCandidate candidate -> do
                           addIceCandidate connection =<< toJSIceCandidate candidate
                           pure Nothing
                         API.MsgCloseConnection  -> do
                           close connection
                           pure Nothing
                     ) (config^.configGotMessage)

  let remoteClosed = push (\msg ->
                             case msg of
                               API.MsgCloseConnection -> pure . Just $ ()
                               _                      -> pure Nothing
                          ) (config^.configGotMessage)

  answers <- push (pure . id) <$> performEvent actions
  pure (answers, remoteClosed)

makeGonimoRTCConnection :: MonadJSM m => m RTCPeerConnection
makeGonimoRTCConnection = liftJSM $ do
  config <- JS.obj
  config <# ("urls" :: Text) $ JS.toJSVal [gonimoTurnServer]
  config <# ("username" :: Text)$ JS.toJSVal gonimoTurnUser
  config <# ("credential" :: Text) $ JS.toJSVal gonimoTurnPassword
  config <# ("credentialType" :: Text) $ JS.toJSVal gonimoTurnCredentialType
  let configDic = case config of JS.Object val -> Dictionary val
  newRTCPeerConnection $ Just configDic

