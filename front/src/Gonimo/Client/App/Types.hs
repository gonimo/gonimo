{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Gonimo.Client.App.Types where

import           Control.Lens
import           Control.Monad
import           Data.Map                      (Map)
import qualified Data.Set                      as Set

import           Gonimo.Client.Account         (Account, HasAccount)
import qualified Gonimo.Client.Account         as Account
import           Gonimo.Client.Auth            (Auth, HasAuth)
import qualified Gonimo.Client.Auth            as Auth
import           Gonimo.Client.Environment     (Environment,
                                                HasEnvironment (..))
import           Gonimo.Client.Host            (HasHost (..), Host)
import qualified Gonimo.Client.Host            as Host
import           Gonimo.Client.Prelude
import           Gonimo.Client.Router          (HasRouter, Router)
import qualified Gonimo.Client.Router          as Router
import           Gonimo.Client.Server          (HasServer, Server)
import qualified Gonimo.Client.Server          as Server
import           Gonimo.Client.Settings        (HasSettings, Settings)
import qualified Gonimo.Client.Settings        as Settings
import qualified Gonimo.Client.Subscriber      as Subscriber
import           Gonimo.Client.Subscriber.Impl (SubscriptionsDyn)
import           Gonimo.I18N
import qualified Gonimo.SocketAPI              as API
import qualified Gonimo.SocketAPI.Types        as API
import qualified Gonimo.Types                  as Gonimo



data ModelConfig t
  = ModelConfig { _accountConfig    :: Account.Config t
                , _subscriberConfig :: Subscriber.Config t
                , _serverConfig     :: Server.Config t
                , _settingsConfig   :: Settings.Config t
                , _routerConfig     :: Router.Config t
                , _hostConfig       :: Host.Config t
                } deriving (Generic)

data Model t
  = Model { __server      :: Server t
          , __account     :: Account t
          , __auth        :: Auth t
          , __settings    :: Settings t
          , __environment :: Environment t
          , __router      :: Router t
          , __host        :: Host t
          }

-- | TODO: Get rid of this.
data Loaded t
  = Loaded { _authData       :: Dynamic t API.AuthData
           , _families       :: Dynamic t (Map API.FamilyId API.Family)
           , _selectedFamily :: Dynamic t API.FamilyId
           }

-- | TODO: Get rid of this, it got replaced by ModelConfig.
--
--   UI modules will simply return polymorphic values like Account.HasConfig or
--   Server.HasConfig. ModelConfig implements all those classes, so by requiring
--   Monoid, UI modules can easily build up the ModelConfig. By leaving the value
--   polymorphic we don't loose modularity, because each UI module will only
--   require the properties it wants to set. This way on can easily test a UI
--   module which only needs Server.HasConfig by forcing the return value to be
--   Server.Config.
data App t
  = App { _subscriptions :: SubscriptionsDyn t
        , _request       :: Event t [ API.ServerRequest ]
        , _selectLang    :: Event t Locale
        }

-- | TODO: Get rid of this, it got replaced by 'ModelConfig'.
data Screen t
  = Screen { _screenApp    :: App t
           , _screenGoHome :: Event t ()
           }


-- Legacy App t to ModelConfig converter.
appToModelConfig :: Reflex t => App t -> ModelConfig t
appToModelConfig app' =
  mempty & Subscriber.subscriptions .~ _subscriptions app'
         & Server.onRequest .~ _request app'
         & Settings.onSelectLocale .~ _selectLang app'

-- Legacy Screen to ModelConfig converter.
screenToModelConfig :: Reflex t => Screen t -> ModelConfig t
screenToModelConfig screen =
  appToModelConfig (_screenApp screen) & Router.onGoBack .~ _screenGoHome screen

type HasModel model t = (HasServer model, HasAccount model, HasAuth model, HasEnvironment model, HasSettings model, HasRouter model, HasHost model)

instance HasServer Model where
  server = _server

instance HasAccount Model where
  account = _account

instance HasAuth Model where
  auth = _auth

instance HasSettings Model where
  settings = _settings

instance HasEnvironment Model where
  environment = _environment

instance HasRouter Model where
  router = _router

instance HasHost Model where
  host = _host

instance (Reflex t) => Default (App t) where
  def = App (constDyn Set.empty) never never

instance (Reflex t) => Default (Screen t) where
  def = Screen def never

instance Reflex t => Semigroup (ModelConfig t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (ModelConfig t) where
  mempty = memptydefault
  mappend = (<>)

instance Reflex t => Default (ModelConfig t) where
  def = mempty

instance Account.HasConfig ModelConfig where
  config = accountConfig

instance Subscriber.HasConfig ModelConfig where
  config = subscriberConfig

instance Server.HasConfig ModelConfig where
  config = serverConfig

instance Settings.HasConfig ModelConfig where
  config = settingsConfig

instance Router.HasConfig ModelConfig where
  config = routerConfig

instance Host.HasConfig ModelConfig where
  config = hostConfig

instance Flattenable ModelConfig where
  flattenWith doSwitch ev
    = ModelConfig
      <$> flattenWith doSwitch (_accountConfig <$> ev)
      <*> flattenWith doSwitch (_subscriberConfig <$> ev)
      <*> flattenWith doSwitch (_serverConfig <$> ev)
      <*> flattenWith doSwitch (_settingsConfig <$> ev)
      <*> flattenWith doSwitch (_routerConfig <$> ev)
      <*> flattenWith doSwitch (_hostConfig <$> ev)

appSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (App t) -> App t
appSwitchPromptlyDyn ev
  = App { _subscriptions = join $ _subscriptions <$> ev
        , _request = switchPromptlyDyn $ _request <$> ev
        , _selectLang = switchPromptlyDyn $ _selectLang <$> ev
        }

screenSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (Screen t) -> Screen t
screenSwitchPromptlyDyn ev
  = Screen { _screenApp = appSwitchPromptlyDyn (_screenApp <$> ev)
           , _screenGoHome = switchPromptlyDyn $ _screenGoHome <$> ev
           }

instance Flattenable App where
  flattenWith doSwitch ev = do
    updatedSubscriptions <- doSwitch never (updated . _subscriptions <$> ev)
    _subscriptions <- holdDyn Set.empty updatedSubscriptions
    _request <- doSwitch never (_request <$> ev)
    _selectLang <- doSwitch never (_selectLang <$> ev)
    pure $ App{..}


instance Flattenable Screen where
  flattenWith doSwitch ev = do
    Screen <$> flattenWith doSwitch (_screenApp <$> ev)
           <*> doSwitch never (_screenGoHome <$> ev)

currentFamilyName :: forall t. Reflex t => Loaded t -> Dynamic t Text
currentFamilyName loaded =
    let
      getFamilyName :: API.FamilyId -> Map API.FamilyId API.Family -> Text
      getFamilyName fid families' = families'^.at fid._Just.to API.familyName . to Gonimo.familyNameName
    in
      zipDynWith getFamilyName (loaded^.selectedFamily) (loaded^.families)

babyNames :: forall t. Reflex t => Loaded t -> Dynamic t [Text]
babyNames loaded =
    let
      getBabyNames :: API.FamilyId -> Map API.FamilyId API.Family -> [Text]
      getBabyNames fid families' = families'^.at fid._Just.to API.familyLastUsedBabyNames
    in
      zipDynWith getBabyNames (loaded^.selectedFamily) (loaded^.families)


-- Auto generated lenses:

-- Lenses for ModelConfig t:

accountConfig :: Lens' (ModelConfig t) (Account.Config t)
accountConfig f modelConfig' = (\accountConfig' -> modelConfig' { _accountConfig = accountConfig' }) <$> f (_accountConfig modelConfig')

subscriberConfig :: Lens' (ModelConfig t) (Subscriber.Config t)
subscriberConfig f modelConfig' = (\subscriberConfig' -> modelConfig' { _subscriberConfig = subscriberConfig' }) <$> f (_subscriberConfig modelConfig')

serverConfig :: Lens' (ModelConfig t) (Server.Config t)
serverConfig f modelConfig' = (\serverConfig' -> modelConfig' { _serverConfig = serverConfig' }) <$> f (_serverConfig modelConfig')

settingsConfig :: Lens' (ModelConfig t) (Settings.Config t)
settingsConfig f modelConfig' = (\settingsConfig' -> modelConfig' { _settingsConfig = settingsConfig' }) <$> f (_settingsConfig modelConfig')

routerConfig :: Lens' (ModelConfig t) (Router.Config t)
routerConfig f modelConfig' = (\routerConfig' -> modelConfig' { _routerConfig = routerConfig' }) <$> f (_routerConfig modelConfig')

hostConfig :: Lens' (ModelConfig t) (Host.Config t)
hostConfig f modelConfig' = (\hostConfig' -> modelConfig' { _hostConfig = hostConfig' }) <$> f (_hostConfig modelConfig')




-- Lenses for Model t:

_server :: Lens' (Model t) (Server t)
_server f model' = (\_server' -> model' { __server = _server' }) <$> f (__server model')

_account :: Lens' (Model t) (Account t)
_account f model' = (\_account' -> model' { __account = _account' }) <$> f (__account model')

_auth :: Lens' (Model t) (Auth t)
_auth f model' = (\_auth' -> model' { __auth = _auth' }) <$> f (__auth model')

_settings :: Lens' (Model t) (Settings t)
_settings f model' = (\_settings' -> model' { __settings = _settings' }) <$> f (__settings model')

_environment :: Lens' (Model t) (Environment t)
_environment f model' = (\_environment' -> model' { __environment = _environment' }) <$> f (__environment model')

_router :: Lens' (Model t) (Router t)
_router f model' = (\_router' -> model' { __router = _router' }) <$> f (__router model')

_host :: Lens' (Model t) (Host t)
_host f model' = (\_host' -> model' { __host = _host' }) <$> f (__host model')



-- Lenses for Loaded t:

authData :: Lens' (Loaded t) (Dynamic t API.AuthData)
authData f loaded' = (\authData' -> loaded' { _authData = authData' }) <$> f (_authData loaded')

families :: Lens' (Loaded t) (Dynamic t (Map API.FamilyId API.Family))
families f loaded' = (\families' -> loaded' { _families = families' }) <$> f (_families loaded')

selectedFamily :: Lens' (Loaded t) (Dynamic t API.FamilyId)
selectedFamily f loaded' = (\selectedFamily' -> loaded' { _selectedFamily = selectedFamily' }) <$> f (_selectedFamily loaded')


-- Lenses for App t:

subscriptions :: Lens' (App t) (SubscriptionsDyn t)
subscriptions f app' = (\subscriptions' -> app' { _subscriptions = subscriptions' }) <$> f (_subscriptions app')

request :: Lens' (App t) (Event t [ API.ServerRequest ])
request f app' = (\request' -> app' { _request = request' }) <$> f (_request app')

selectLang :: Lens' (App t) (Event t Locale)
selectLang f app' = (\selectLang' -> app' { _selectLang = selectLang' }) <$> f (_selectLang app')


-- Lenses for Screen t:

screenApp :: Lens' (Screen t) (App t)
screenApp f screen' = (\screenApp' -> screen' { _screenApp = screenApp' }) <$> f (_screenApp screen')

screenGoHome :: Lens' (Screen t) (Event t ())
screenGoHome f screen' = (\screenGoHome' -> screen' { _screenGoHome = screenGoHome' }) <$> f (_screenGoHome screen')


