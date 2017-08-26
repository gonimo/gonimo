{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Reflex.Dom.WebSocket.Types where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import GHCJS.DOM.Types (MonadJSM)

import Control.Lens
import Data.Default
import Data.Text
import qualified GHCJS.DOM.WebSocket as JS
import qualified Data.Text as T
import Control.Monad.Fix



data CloseParams
  = CloseParams {
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                  _closeCode :: Word
                  -- | Parameter for the JS close method.
                  -- See: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#close()
                , _closeReason :: Text
                }

data Config t
   = Config { _configSend :: Event t [Text]
            , _configClose :: Event t CloseParams
              -- | Timeout in seconds we wait for the JavaScript WebSocket connection
              -- to send it's close event, before we disconnect all event handlers and trigger
              -- '_webSocket_close' ourselves. Pass 'Nothing' if you want to
              -- wait indefinitely for the JS implementation.
            , _configCloseTimeout :: Maybe Word
            }

data WebSocket t
   = WebSocket { _open :: Event t ()
               , _receive :: Event t Text
               -- | error event does not carry any data and is always
               -- followed by termination of the connection
               -- for details see the close event
               , _error :: Event t ()
               , _close :: Event t ( Bool -- ^ wasClean
                                   , CloseParams
                                   )
               -- Internal stuff starts here:
               , _ws :: Dynamic t JS.WebSocket -- ^ JavaScript WebSocket object
               -- | Close event from JS WebSocket.
               , _jsClose :: Event t ( Bool
                                     , CloseParams
                                     )
               , _triggerOpen :: IO ()
               , _triggerClose :: (Bool, CloseParams) -> IO ()
               , _triggerReceive :: Text -> IO ()
               , _triggerError :: IO ()
               }



makeLenses ''Config
makeLenses ''WebSocket
makeLenses ''CloseParams



type WebSocketM t m = (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadFix m, MonadHold t m)


instance Default CloseParams where
  def = CloseParams { _closeCode = 1000
                    , _closeReason = T.empty
                    }


instance Reflex t => Default (Config t) where
  def = Config { _configSend = never
               , _configClose = never
               , _configCloseTimeout = Nothing
               }
