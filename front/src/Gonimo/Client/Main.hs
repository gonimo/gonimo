{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gonimo.Client.Main where

import           Control.Concurrent

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Aeson                  as Aeson
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.History           as History
import qualified GHCJS.DOM.Location          as Location
import           GHCJS.DOM.Types             (MonadJSM, liftJSM, toJSVal)
import qualified GHCJS.DOM.Window            as Window
import qualified Language.Javascript.JSaddle as JS
import           Network.HTTP.Types          (urlDecode)
import           Reflex.Dom.Core             hiding (webSocketConfig_reconnect,
                                              webSocketConfig_send)

import qualified Gonimo.Client.Account       as Account
import           Gonimo.Client.App           as App
import qualified Gonimo.Client.Auth          as Auth
import qualified Gonimo.Client.Config        as Config
import           Gonimo.Client.I18N
import           Gonimo.Client.Prelude       hiding (app)
import qualified Gonimo.Client.Server        as Server
import qualified Gonimo.Client.Storage       as GStorage
import qualified Gonimo.Client.Storage.Keys  as GStorage
import qualified Gonimo.Client.Subscriber    as Subscriber
import           Gonimo.I18N
import           Gonimo.Types                (InvitationSecret)

data Config
  = Config { -- | Have the app accept an invitation.
             _newInvitation :: MVar InvitationSecret
           }

mkEmptyConfig :: IO Config
mkEmptyConfig = do
  Config <$> newEmptyMVar

app :: forall t. Config -> Widget t ()
app conf' = build $ \appModel -> do
  conf <- toModelConfig conf'

  liftIO $ putStrLn "Loaded - yeah!"

  let runApp = flip runReaderT (GonimoEnv (appModel ^. gonimoLocale)) $ ui appModel
  -- Little hack for now: We simply delay the UI a bit, so we avoid "blocked on MVar".
  app' <- networkViewFlatten (pure runApp)

  fullAuth <- Auth.make (appModel ^. gonimoLocale) appModel

  (accountConf, account') <- Account.make appModel
                             $ conf <> app'

  subscriberServerConfig <- Subscriber.make appModel
                            $ app' <> accountConf

  initLang      <- readLocale
  currentLocale <- holdDyn initLang $ app'^.selectLanguage
  performEvent_ $ writeLocale <$> updated (appModel ^. gonimoLocale)

  server' <- Server.make Config.gonimoBackWSURL
             $ fullAuth^.Server.config
             <> subscriberServerConfig
             <> app' ^. serverConfig
             <> accountConf ^. Server.config


  pure $ Model { __auth            = fullAuth^.Auth._auth
               , __account         = account'
               , __server          = server'
               , App._gonimoLocale = currentLocale
               }

  where
    build = void . mfix

main :: Config -> JS.JSM ()
-- main = run 3709 $ mainWidget app
main conf = do
    fmap (fromMaybe ()) . runMaybeT $ do
      invSecret <- getInvitationSecret
      clearInvitationFromURL
      runOnce $ putMVar (conf ^. newInvitation) invSecret
    mainWidgetInElementById "app" $ app conf
  where
    runOnce = liftIO . void . forkIO


toModelConfig :: (Reflex t, MonadIO m, TriggerEvent t m)
              => Config -> m (ModelConfig t)
toModelConfig conf = do
  (onClaimInvitation', sendInvitation) <- newTriggerEvent
  run $ do
    inv <- takeMVar $ conf ^. newInvitation
    sendInvitation inv
  pure $ mempty & Account.onClaimInvitation .~ fmap (:[]) onClaimInvitation'
  where
    run = liftIO . void . forkIO . forever

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

getInvitationSecret :: forall m. (MonadPlus m, MonadJSM m) => m InvitationSecret
getInvitationSecret = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    queryString <- Location.getSearch location
    let secretString =
          let
            (_, startSecret) = T.drop 1 <$> T.breakOn "=" queryString
          in
            T.takeWhile (/='&') startSecret
    guard $ not (T.null secretString)
    let mDecoded = Aeson.decodeStrict . urlDecode True . T.encodeUtf8 $ secretString
    maybe mzero pure $ mDecoded

clearInvitationFromURL :: forall m. (MonadJSM m) => m ()
clearInvitationFromURL = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    history <- Window.getHistory window
    href <- Location.getHref location
    emptyJSVal <- liftJSM $ toJSVal T.empty
    History.pushState history emptyJSVal ("gonimo" :: Text) (Just $ T.takeWhile (/='?') href)

-- Generated lenses:


-- Lenses for Config:

newInvitation :: Lens' Config (MVar InvitationSecret)
newInvitation f config' = (\newInvitation' -> config' { _newInvitation = newInvitation' }) <$> f (_newInvitation config')
