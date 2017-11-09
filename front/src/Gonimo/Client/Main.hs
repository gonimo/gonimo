{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Gonimo.Client.Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified GHCJS.DOM                   as DOM
import           GHCJS.DOM.Types             (MonadJSM, liftJSM)
import qualified GHCJS.DOM.Window            as Window
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom.Core             hiding (webSocketConfig_reconnect,
                                              webSocketConfig_send)
import qualified Gonimo.Client.App          as App
import qualified Gonimo.Client.Auth         as Auth
import qualified Gonimo.Client.Config       as Config
import           Gonimo.Client.I18N
import           Gonimo.Client.Server
import qualified Gonimo.Client.Server       as Server
import qualified Gonimo.Client.Storage      as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified Gonimo.Client.Subscriber   as Subscriber
import           Gonimo.I18N

-- import qualified GHCJS.DOM.Types as JS

app :: forall x. Widget x ()
app = mdo
  liftIO $ putStrLn "Loaded - yeah!"
  let serverRequests = auth^.Auth.request
                    <> subscriber^.Subscriber.request
                    <> app^.App.request

  let serverConfig' = def & configRequest .~ serverRequests

  server' <- Server.create Config.gonimoBackWSURL serverConfig'

  let authConfig = Auth.Config { Auth._configResponse = server'^.response
                               , Auth._configServerOpen = server'^.open
                               , Auth._configServerClose = const () <$> server'^.close
                               , Auth._configServerCloseRequested = server'^.closeRequested
                               }
  auth <- Auth.auth currentLocale authConfig

  let subscriberConfig
        = Subscriber.Config { Subscriber._configResponse = server'^.response
                            , Subscriber._configSubscriptions = app^.App.subscriptions
                            , Subscriber._configAuthenticated = auth^.Auth.authenticated
                            }
  subscriber <- Subscriber.subscriber subscriberConfig

  let appConfig = App.Config { App.__server = server'
                             , App._subscriber = subscriber
                             , App._auth = auth
                             }
  app <- flip runReaderT (GonimoEnv currentLocale) $ App.ui appConfig
  initLang <- readLocale
  currentLocale <- holdDyn initLang $ app^.App.selectLang
  performEvent_ $ writeLocale <$> updated currentLocale
  pure ()

main :: JS.JSM ()
-- main = run 3709 $ mainWidget app
main = mainWidgetInElementById "app" app



localeFromBrowserString :: Text -> Locale
localeFromBrowserString langStr
  | T.isPrefixOf "de" langStr = DE_DE
  | otherwise = EN_GB

readLocale :: MonadJSM m => m Locale
readLocale = do
  storage <- liftJSM $ Window.getLocalStorage =<< DOM.currentWindowUnchecked
  browserLocaleStr <- liftJSM $ fromMaybe "en-US" <$> (JS.fromJSVal =<< JS.eval ("navigator.language" :: Text))
  let browserLocale = localeFromBrowserString browserLocaleStr
  fromMaybe browserLocale <$> GStorage.getItem storage GStorage.userLocale

writeLocale :: MonadJSM m => Locale -> m ()
writeLocale lastBabyName = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.userLocale lastBabyName
