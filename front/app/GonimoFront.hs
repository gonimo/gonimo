{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import           Control.Lens
import           Data.Monoid
import           Control.Monad.IO.Class
import qualified Gonimo.Client.App        as App
import qualified Gonimo.Client.Auth       as Auth
import qualified Gonimo.Client.Config     as Config
import           Gonimo.Client.Server     (webSocketConfig_send, webSocket_open,
                                           webSocket_recv)
import qualified Gonimo.Client.Server     as Server
import qualified Gonimo.Client.Subscriber as Subscriber
import           Reflex.Dom.Core               hiding (webSocketConfig_send)

import Language.Javascript.JSaddle.Warp (run)
import qualified Language.Javascript.JSaddle              as JS
import            Data.Text (Text)



app :: forall x. Widget x ()
app = mdo
  liftIO $ putStrLn "Loaded - yeah!"
  let serverRequests = auth^.Auth.request
                    <> subscriber^.Subscriber.request
                    <> app^.App.request

  let wsConfig = def & webSocketConfig_send .~ serverRequests
  server <- Server.server Config.gonimoBackWSURL  wsConfig

  let authConfig = Auth.Config { Auth._configResponse = server^.webSocket_recv
                               , Auth._configServerOpen = server^.webSocket_open
                               }
  auth <- Auth.auth authConfig

  let subscriberConfig
        = Subscriber.Config { Subscriber._configResponse = server^.webSocket_recv
                            , Subscriber._configSubscriptions = app^.App.subscriptions
                            , Subscriber._configAuthenticated = auth^.Auth.authenticated
                            }
  subscriber <- Subscriber.subscriber subscriberConfig

  let appConfig = App.Config { App._server = server
                             , App._subscriber = subscriber
                             , App._auth = auth
                             }
  app <- App.ui appConfig
  pure ()

main :: IO ()
main = do
  -- putStrLn "Run it baby!"
  -- run 3709 $ do
  --   liftIO $ putStrLn "Before console.log"
  --   _ <- JS.eval("console.log('ehehehehe we are here!');" :: Text);
  --   liftIO $ putStrLn "After console.log"
  --   pure ()
  run 3709 $ mainWidgetInElementById "app" app

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
