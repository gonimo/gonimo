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
import qualified Language.Javascript.JSaddle   as JS
import           Reflex.Dom.Core               hiding
                                                (webSocketConfig_reconnect,
                                                webSocketConfig_send)

import qualified Gonimo.Client.Account.Impl    as Account
import           Gonimo.Client.App             as App
import qualified Gonimo.Client.Auth.Impl       as Auth
import qualified Gonimo.Client.Environment     as Environment
import           Gonimo.Client.Prelude         hiding (app)
import qualified Gonimo.Client.Router.Impl     as Router
import qualified Gonimo.Client.Server          as Server
import qualified Gonimo.Client.Settings        as Settings
import qualified Gonimo.Client.Subscriber.Impl as Subscriber
import           Gonimo.Types                  (InvitationSecret)


-- | Configuration coming from the outside.
--
--   This is a bit ugly right now, because the reflex-dom mainWidget and run
--   functions expect () as return value, therefore we can't simply return a
--   data types with callback functions, but instead we need to pass in this
--   'Config' which contains MVars that can be set by out side code. This is
--   needed at the moment on Android for passing in Intents (Invitation URLs for
--   the time being) at runtime.
data Config
  = Config { -- | Have the app accept an invitation.
             _newInvitation :: MVar InvitationSecret
           }

-- | Make an empty 'Config'.
mkEmptyConfig :: IO Config
mkEmptyConfig = do
  Config <$> newEmptyMVar

-- | What does our application need, well here it is ... ;-)
type AppConstraint t m = MonadWidget t m
  -- = ( DomBuilder t m, MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
  --   , HasJSContext m, PerformEvent t m, TriggerEvent t m
  --   , PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadSample t (Performable m)
  --   , Ref m ~ IORef
  --   )

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

  __environment            <- Environment.make

  __router                 <- Router.make modelConf

  __server                 <- Server.make model modelConf

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
main conf = mainWidgetInElementById "app" $ app conf


-- | Get a 'ModelConfig' from our 'MVar' Config.
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


-- Generated lenses:

-- Lenses for Config:

newInvitation :: Lens' Config (MVar InvitationSecret)
newInvitation f config' = (\newInvitation' -> config' { _newInvitation = newInvitation' }) <$> f (_newInvitation config')
