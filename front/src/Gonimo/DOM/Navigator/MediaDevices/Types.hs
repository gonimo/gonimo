module Gonimo.DOM.Navigator.MediaDevices.Types where

import           Control.Monad
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import           Data.Text                   (Text)

import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           GHCJS.Marshal               (FromJSVal, ToJSVal, fromJSVal,
                                              fromJSValUnchecked, toJSVal)
import           Language.Javascript.JSaddle (JSM, JSVal, MonadJSM, eval, fun,
                                              js, js1, jsg, jss, liftJSM,
                                              syncPoint, valToNumber, (!),
                                              ( # ))

data MediaDeviceKind = AudioInput | VideoInput | AudioOutput deriving (Show, Eq)


data MediaDeviceInfo
  = MediaDeviceInfo { mediaDeviceDeviceId :: Text
                    , mediaDeviceKind :: MediaDeviceKind
                    , mediaDeviceLabel :: Text
                    , mediaDeviceGroupId :: Text
                    } deriving (Show)


instance FromJSVal MediaDeviceKind where
  fromJSVal val = runMaybeT $ do
    t :: Text <- MaybeT $ fromJSVal val
    case t of
      "audioinput" -> pure AudioInput
      "audiooutput" -> pure AudioOutput
      "videoinput" -> pure VideoInput
      _ -> mzero


instance ToJSVal MediaDeviceKind where
  toJSVal val = do
    let
      textToJSVal :: Text -> JSM JSVal
      textToJSVal = toJSVal
    case val of
      AudioInput -> textToJSVal "audioinput"
      VideoInput -> textToJSVal "videoinput"
      AudioOutput -> textToJSVal "audiooutput"
