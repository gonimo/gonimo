{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedStrings #-}
-- Functions for translating API data types to types used in frontend code:
module Gonimo.SocketAPI.Translations where

import qualified GHCJS.DOM.Enums                 as F
import           GHCJS.DOM.RTCIceCandidate
import           GHCJS.DOM.RTCSessionDescription
import           GHCJS.DOM.Types                 (MonadJSM,
                                                  RTCIceCandidateInit (..),
                                                  RTCSessionDescriptionInit (..))
import qualified Gonimo.SocketAPI.Types          as API
import           Language.Javascript.JSaddle     (liftJSM, (<#))
import qualified Language.Javascript.JSaddle     as JS


-- Types than can be translated one to one. Types are isomorph!
class MFrontendTranslation api front | api -> front where
  mToFrontend :: forall m. MonadJSM m => api -> m front
  mFromFrontend :: forall m. MonadJSM m => front -> m api

class FrontendTranslation api front | api -> front where
  toFrontend :: api -> front
  fromFrontend :: front -> api

-- instance FrontendTranslation api front => MFrontendTranslation api front where
--   mToFrontend = pure . toFrontend

--   mFromFrontend = pure . fromFrontend


instance FrontendTranslation API.RTCSdpType F.RTCSdpType where
  toFrontend api
    = case api of
        API.RTCSdpTypeOffer -> F.RTCSdpTypeOffer
        API.RTCSdpTypePranswer -> F.RTCSdpTypePranswer
        API.RTCSdpTypeAnswer -> F.RTCSdpTypeAnswer
        API.RTCSdpTypeRollback -> F.RTCSdpTypeRollback

  fromFrontend f
    = case f of
        F.RTCSdpTypeOffer -> API.RTCSdpTypeOffer
        F.RTCSdpTypePranswer -> API.RTCSdpTypePranswer
        F.RTCSdpTypeAnswer -> API.RTCSdpTypeAnswer
        F.RTCSdpTypeRollback -> API.RTCSdpTypeRollback


instance MFrontendTranslation API.IceCandidate RTCIceCandidateInit where
  mToFrontend (API.IceCandidate index mid candidate) = liftJSM $ do
    rawDic <- JS.obj
    rawDic <# "sdpMLineIndex" $ index
    rawDic <# "sdpMid" $ mid
    rawDic <# "candidate" $ candidate
    pure $ case rawDic of JS.Object val -> RTCIceCandidateInit val

  mFromFrontend rtcCandidateInit = do 
    rtcCandidate <- newRTCIceCandidate rtcCandidateInit
    API.IceCandidate
      <$> fmap (fmap fromIntegral) (getSdpMLineIndex rtcCandidate)
      <*> getSdpMid rtcCandidate
      <*> getCandidate rtcCandidate

instance MFrontendTranslation API.SessionDescription RTCSessionDescriptionInit where
  mToFrontend (API.SessionDescription sdp type_) = liftJSM $ do
    rawDic <- JS.obj
    rawDic <# "sdp" $ sdp
    rawDic <# "type" $ toFrontend type_
    pure $ case rawDic of JS.Object val -> RTCSessionDescriptionInit val

  mFromFrontend rtcSessionInit = liftJSM $ do
    rtcSession <- newRTCSessionDescription rtcSessionInit
    sdp <- getSdp rtcSession
    type_ <- getType rtcSession
    pure $ API.SessionDescription sdp (fromFrontend type_)
