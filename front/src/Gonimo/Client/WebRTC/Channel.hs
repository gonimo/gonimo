{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.WebRTC.Channel where

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
import           GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import           GHCJS.DOM.RTCPeerConnection       hiding (newRTCPeerConnection)
import           GHCJS.DOM.EventTarget
import qualified GHCJS.DOM.RTCPeerConnection       as RTCPeerConnection
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.Db.Entities                as Db
import           Reflex.Dom
import qualified Gonimo.SocketAPI.Types            as API

import           GHCJS.DOM.Types                   ( MediaStream, MonadJSM, Dictionary(..)
                                                   , EventListener(..))
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.Client.Auth                as Auth
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)
import           Gonimo.DOM.Navigator.MediaDevices
import           Gonimo.DOM.Window                 (newRTCPeerConnection)
import           Gonimo.Client.Config
import           Gonimo.Client.Prelude
import           Gonimo.Client.Util

import           Language.Javascript.JSaddle                       (JSVal,
                                                                    eval, fun,
                                                                    function,
                                                                    js, js1,
                                                                    jsf, jsg,
                                                                    jss, js0,
                                                                    liftJSM,
                                                                    syncPoint,
                                                                    valToNumber,
                                                                    (<#))
import qualified Language.Javascript.JSaddle                       as JS

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

  -- let isBabyStation = isJust ourStream'
  -- alarm' <- if isBabyStation
  --           then  Just <$> loadSound "/sounds/pup_alert.mp3"
  --           else pure Nothing
  pure $ Channel { _sendMessage = undefined
                 , _rtcConnection = conn
                 , _ourStream = ourStream'
                 , _remoteStream = undefined
                 , _closed = undefined
                 }


makeGonimoRTCConnection :: MonadJSM m => m RTCPeerConnection
makeGonimoRTCConnection = liftJSM $ do
  config <- JS.obj
  config <# ("urls" :: Text) $ JS.toJSVal [gonimoTurnServer]
  config <# ("username" :: Text)$ JS.toJSVal gonimoTurnUser
  config <# ("credential" :: Text) $ JS.toJSVal gonimoTurnPassword
  config <# ("credentialType" :: Text) $ JS.toJSVal gonimoTurnCredentialType
  let configDic = case config of JS.Object val -> Dictionary val
  newRTCPeerConnection $ Just configDic

