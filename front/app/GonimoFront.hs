{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom hiding (webSocketConfig_send)
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
import qualified Gonimo.Client.Invite as Invite
import qualified Gonimo.Client.MessageBox as MessageBox
import qualified Gonimo.Client.AcceptInvitation as AcceptInvitation
import qualified Gonimo.Client.Family as Family
import qualified Gonimo.Client.Subscriber as Subscriber
import Control.Monad



main :: IO ()
main = mainWidgetInElementById "app" $ mdo
  let serverRequests = auth^.Auth.request
                    <> subscriber^.Subscriber.request
                    <> family^.Family.request
                    <> accept^.AcceptInvitation.request

  let wsConfig = def & webSocketConfig_send .~ serverRequests
  server <- Server.server "ws://localhost:8081" wsConfig

  let authConfig = Auth.Config { Auth._configResponse = server^.webSocket_recv
                               , Auth._configServerOpen = server^.webSocket_open
                               }
  auth <- Auth.auth authConfig

  let subscriberConfig = Subscriber.Config { Subscriber._configResponse = server^.webSocket_recv
                                           , Subscriber._configSubscriptions = family^.Family.subscriptions
                                           , Subscriber._configAuthenticated = auth^.Auth.authenticated
                                           }
  subscriber <- Subscriber.subscriber subscriberConfig

  let messageBoxConfig
        = MessageBox.Config { MessageBox._configMessage = (:[]) . MessageBox.ServerResponse <$> server^.webSocket_recv
                            }
  msgBox <- MessageBox.ui messageBoxConfig
  let msgSwitchFamily = push (\actions -> case actions of
                                 [MessageBox.SelectFamily fid] -> pure $ Just fid
                                 _ -> pure Nothing -- Dirty: We ignore selectfamily if multiple events occurred ...
                             ) (msgBox ^. MessageBox.action)

  let acceptConfig
        = AcceptInvitation.Config { AcceptInvitation._configResponse = server^.webSocket_recv
                                  , AcceptInvitation._configAuthenticated = auth^.Auth.authenticated
                                  }
  accept <- AcceptInvitation.ui acceptConfig

  let familyConfig = Family.Config { Family._configResponse = server^.webSocket_recv
                                   , Family._configAuthData = auth^.Auth.authData
                                   , Family._configSelectFamily = msgSwitchFamily
                                   , Family._configAuthenticated = auth^.Auth.authenticated
                                   , Family._configCreateFamily = never
                                   , Family._configLeaveFamily = never
                                   }
  family <- Family.ui familyConfig
  pure ()

-- headTag :: forall x. Widget x ()
-- headTag = do
--   forM_ [ "//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" --TODO Make these links local
--         , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
--         , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
--         ] $ \x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) $ pure ()
--   forM_ [ "//ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"
--         , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
--         ] $ \x -> elAttr "script" ("src" =: x) $ pure ()
--   elAttr "meta" ("name" =: "viewport"
--                  <> "content" =: "width=device-width, initial-scale=1, user-scalable=no"
--                 ) $ pure ()
