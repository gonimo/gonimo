{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.DOM.Window.Internal where


import           Control.Concurrent.MVar                           (newEmptyMVar,
                                                                    putMVar,
                                                                    takeMVar)
import           Control.Monad                                     (void)
import           GHCJS.DOM.Types                                   (fromJSValUnchecked)
import           Language.Javascript.JSaddle                       (JSVal,
                                                                    MonadJSM,
                                                                    eval, fun,
                                                                    js, js1,
                                                                    jsf, jsg,
                                                                    jss, js0,
                                                                    liftJSM,
                                                                    syncPoint,
                                                                    valToNumber,
                                                                    new,
                                                                    ( # ), (<#))
import qualified Language.Javascript.JSaddle                       as JS


import           Control.Lens                                      ((^.))
import           Control.Monad.IO.Class
import           Data.Maybe                                        (fromJust)
import           GHCJS.DOM.Types                                   hiding
                                                                    (MonadJSM,
                                                                    liftJSM)

import           Control.Concurrent.MVar                           (newEmptyMVar,
                                                                    putMVar,
                                                                    takeMVar)
import           Control.Monad.IO.Class                            (MonadIO (..))


import           GHCJS.DOM.NavigatorUserMediaError              (throwUserMediaException)

import           GHCJS.DOM.NavigatorUserMediaErrorCallback (newNavigatorUserMediaErrorCallback)
import           GHCJS.DOM.NavigatorUserMediaSuccessCallback (newNavigatorUserMediaSuccessCallback)


newRTCPeerConnection :: (MonadJSM m, IsDictionary rtcConfiguration)
                        => Maybe rtcConfiguration -> m RTCPeerConnection
newRTCPeerConnection someDict = liftJSM $ do
  let args = case toDictionary <$> someDict of
        Just (Dictionary d) -> [d]
        _ -> []
  RTCPeerConnection <$> new (jsg "RTCPeerConnection") args
