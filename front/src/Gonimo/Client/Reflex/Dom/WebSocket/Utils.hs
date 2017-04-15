{-# LANGUAGE CPP #-}
{- Stolen and modified from reflex-dom, thanks!

Copyright (c) 2015, Obsidian Systems LLC
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
module Gonimo.Client.Reflex.Dom.WebSocket.Utils
  ( bsFromMutableArrayBuffer
  , bsToArrayBuffer
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified GHCJS.Buffer as JS
import           GHCJS.DOM.Types (ArrayBuffer (..))
import qualified JavaScript.TypedArray.ArrayBuffer as JS
import           Language.Javascript.JSaddle.Types (MonadJSM, liftJSM, ghcjsPure, jsval, JSVal)

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Types (JSVal, JSM)
import Language.Javascript.JSaddle.Object (new, jsg)
import Data.Text (Text)
#endif

{-# INLINABLE bsFromMutableArrayBuffer #-}
bsFromMutableArrayBuffer :: MonadJSM m => JS.MutableArrayBuffer -> m ByteString
bsFromMutableArrayBuffer ab = liftJSM $ JS.unsafeFreeze ab >>=
    ghcjsPure . JS.createFromArrayBuffer >>= ghcjsPure . JS.toByteString 0 Nothing

{-# INLINABLE bsToArrayBuffer #-}
bsToArrayBuffer :: MonadJSM m => ByteString -> m ArrayBuffer
bsToArrayBuffer bs = liftJSM $ do
  (b, off, len) <- ghcjsPure $ JS.fromByteString bs
  ArrayBuffer <$> if BS.length bs == 0 --TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49 is fixed
                  then JS.create 0 >>= ghcjsPure . JS.getArrayBuffer >>= ghcjsPure . jsval
                  else do
                    ref <- ghcjsPure (JS.getArrayBuffer b) >>= ghcjsPure . jsval
                    js_dataView off len ref

#ifdef __GHCJS__

foreign import javascript safe "new DataView($3,$1,$2)"
  js_dataView :: Int -> Int -> JSVal -> IO JSVal

#else

js_dataView :: Int -> Int -> JSVal -> JSM JSVal
js_dataView off len ref = new (jsg ("DataView" :: Text)) (ref, off, len)

#endif
