module Gonimo.Client.Server where

import Gonimo.SocketAPI (ServerRequest, ServerResponse)
import Reflex
import Control.Lens

data ServerConfig t
  = ServerConfig { _serverConfigSend :: Event t [ServerRequest]
                 }

makeLenses ''ServerConfig

data Server t
  = Server { _serverReceive :: Event t ServerResponse
           }

server :: forall t m. (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m, IsWebSocketMessage a) => String -> ServerConfig t -> m (Server t)
server url config 


makeLenses ''ServerConfig
makeLenses ''Server
