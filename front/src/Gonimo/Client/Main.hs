{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Gonimo.Client.Account       as Account
import           Gonimo.Client.App           as App
import qualified Gonimo.Client.Auth          as Auth
import qualified Gonimo.Client.Config        as Config
import           Gonimo.Client.I18N
import qualified Gonimo.Client.Server        as Server
import qualified Gonimo.Client.Storage       as GStorage
import qualified Gonimo.Client.Storage.Keys  as GStorage
import qualified Gonimo.Client.Subscriber    as Subscriber
import           Gonimo.I18N



app :: forall x. Widget x ()
app = build $ \appDeps -> do
  liftIO $ putStrLn "Loaded - yeah!"

  let runApp = flip runReaderT (GonimoEnv (appDeps ^. gonimoLocale)) $ ui appDeps
  -- Little hack for now: We simply delay the UI a bit, so we avoid "blocked on MVar".
  app' <- appSwitchPromptly =<< dyn (pure runApp)

  fullAuth <- Auth.make (appDeps ^. gonimoLocale) appDeps

  let accountConfig
        = Account.Config { Account._onClaimInvitation = never
                         , Account._onAnswerInvitation = never
                         }
  fullAccount <- Account.make appDeps accountConfig

  subscriberServerConfig <- Subscriber.make appDeps
                            $ Subscriber.Config (app'^.subscriptions)

  initLang      <- readLocale
  currentLocale <- holdDyn initLang $ app'^.selectLang
  performEvent_ $ writeLocale <$> updated (appDeps ^. gonimoLocale)

  server' <- Server.make Config.gonimoBackWSURL
             $ fullAuth^.Auth.serverConfig
             <> subscriberServerConfig
             <> Server.Config (app'^.request)


  pure $ Config { __auth            = fullAuth^.Auth._auth
                , __account         = fullAccount^.Account._account
                , __server          = server'
                , App._gonimoLocale = currentLocale
                }

  where
    build = void . mfix

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
