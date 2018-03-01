{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
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
import qualified Gonimo.Client.Settings      as Settings
import           Gonimo.Client.Prelude       hiding (app)
import qualified Gonimo.Client.Server        as Server
import qualified Gonimo.Client.Subscriber    as Subscriber

import           Gonimo.Types                (InvitationSecret)

data Config
  = Config { -- | Have the app accept an invitation.
             _newInvitation :: MVar InvitationSecret
           }

mkEmptyConfig :: IO Config
mkEmptyConfig = do
  Config <$> newEmptyMVar

type AppConstraint t m
  = ( DomBuilder t m, MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
    , HasJSContext m, PerformEvent t m, TriggerEvent t m
    , PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadSample t (Performable m)
    )

-- | Wire up the app.
--
--   We build up the 'Model' and the 'ModelConfig' and wire the components
--   accordingly.
--
--   Each component (part of the model) has it's own view on what the model actually is, It is
--   defined in their module's 'HasModel' constraint and specifies the other
--   components it needs for its own operation. The actual model is kept
--   polymorphic, so we can wire the components up by simply passing in 'Model'
--   which is a collection of all components available. By leaving the model in
--   the components polymorphic we make this great simplification possible,
--   while at the same time, each component can be tested with it's minimal set
--   of requirements.
--
--  'ModelConfig' gets build by the components in a similar way: Each component
--  specifies a 'HasModelConfig' constraint in their module, which specifies
--  precisely which parts of the module the components 'make' function
--  is going to configure. Again by leaving this polymorphic we can test
--  individual components with the minimum of required dependencies, while at
--  the same time we can simply treat it as 'ModelConfig' in this function for all
--  components.
app :: forall t m. AppConstraint t m => Config -> m ()
app conf' = build $ \ ~(modelConf, model) -> do
  liftIO $ putStrLn "Loaded - yeah!"

  conf                     <- toModelConfig conf'

  __server                 <- Server.make Config.gonimoBackWSURL modelConf

  (authConf, __auth)       <- Auth.make model

  (accountConf, __account) <- Account.make model modelConf

  subscriberConf           <- Subscriber.make model modelConf

  __settings               <- Settings.make modelConf

  uiConf                   <- makeUI model

  pure ( mconcat [ conf
                 , authConf
                 , accountConf
                 , subscriberConf
                 , uiConf
                 ]
       , Model {..}
       )
  where
    build :: ((ModelConfig t, Model t) -> m (ModelConfig t, Model t)) -> m ()
    build = void . mfix

    -- Delay UI rendering until network is ready.
    makeUI :: Model t -> m (ModelConfig t)
    makeUI = networkViewFlatten . constDyn . runReaderT ui



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
