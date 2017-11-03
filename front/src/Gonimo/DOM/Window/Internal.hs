{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.DOM.Window.Internal where


import           GHCJS.DOM.Types             hiding (MonadJSM, liftJSM)
import           Language.Javascript.JSaddle (MonadJSM, jsg, liftJSM, new)





newRTCPeerConnection :: (MonadJSM m, IsDictionary rtcConfiguration)
                        => Maybe rtcConfiguration -> m RTCPeerConnection
newRTCPeerConnection someDict = liftJSM $ do
  let args = case toDictionary <$> someDict of
        Just (Dictionary d) -> [d]
        _ -> []
  RTCPeerConnection <$> new (jsg "RTCPeerConnection") args
