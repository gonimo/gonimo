{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom hiding (webSocketConfig_send, webSocket_recv)
import qualified Data.Text as T
import Control.Lens
import Safe
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Gonimo.SocketAPI.Types as API
import qualified GHCJS.DOM.JSFFI.Generated.Storage as Storage
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import Data.Default
import qualified Gonimo.Client.Server as Server
import Gonimo.Client.Server (webSocketConfig_send, webSocket_recv, webSocket_open)
import qualified Gonimo.Client.Auth as Auth
import Gonimo.Client.Auth (AuthConfig(..), Auth, authConfigResponse, authRequest)
import Gonimo.Client.Invite (invite)
import Control.Monad



main :: IO ()
main = mainWidgetWithHead headTag $ mdo
  let wsConfig = def & webSocketConfig_send .~ serverRequests
  server <- Server.server "ws://localhost:8081" wsConfig
  let authConfig = AuthConfig { _authConfigResponse = server^.webSocket_recv
                              , _authConfigServerOpen = server^.webSocket_open
                              }
  auth <- Auth.auth authConfig
  let serverRequests = auth^.authRequest
  invite
  pure ()

headTag :: forall x. Widget x ()
headTag = do
  forM_ [ "//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" --TODO Make these links local
        , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
        ] $ \x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) $ pure ()
  forM_ [ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        , "//ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"
        ] $ \x -> elAttr "script" ("src" =: x) $ pure ()
  elAttr "meta" ("name" =: "viewport"
                 <> "content" =: "width=device-width, initial-scale=1, user-scalable=no"
                ) $ pure ()
