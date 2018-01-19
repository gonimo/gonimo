module Gonimo.Client.App.Types where

import Reflex
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.SocketAPI.Types as API
import Control.Lens
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Subscriber as Subscriber
import Gonimo.Client.Subscriber (SubscriptionsDyn)
import Data.Map (Map)
import Control.Monad
import Gonimo.Client.Prelude
import qualified Gonimo.Types as Gonimo

import qualified Data.Set as Set
import Gonimo.I18N
import Gonimo.Client.Server (Server, HasServer)
import qualified Gonimo.Client.Server as Server

data Config t
  = Config { __server :: Server t
           , _auth :: Auth.Auth t
           , _subscriber :: Subscriber.Subscriber t
           }

data Loaded t
  = Loaded { _authData :: Dynamic t API.AuthData
           , _families :: Dynamic t (Map API.FamilyId API.Family)
           , _selectedFamily :: Dynamic t API.FamilyId
           }

data App t
  = App { _subscriptions :: SubscriptionsDyn t
        , _request :: Event t [ API.ServerRequest ]
        , _selectLang :: Event t Locale
        }

data Screen t
  = Screen { _screenApp :: App t
           , _screenGoHome :: Event t ()
           }

instance HasServer Config where
  server = _server


instance (Reflex t) => Default (App t) where
  def = App (constDyn Set.empty) never never

instance (Reflex t) => Default (Screen t) where
  def = Screen def never

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


-- Config lenses:

_server :: Lens' (Config t) (Server t)
_server f config' = (\_server' -> config' { __server = _server' }) <$> f (__server config')

auth :: Lens' (Config t) (Auth.Auth t)
auth f config' = (\auth' -> config' { _auth = auth' }) <$> f (_auth config')

subscriber :: Lens' (Config t) (Subscriber.Subscriber t)
subscriber f config' = (\subscriber' -> config' { _subscriber = subscriber' }) <$> f (_subscriber config')

-- Loaded lenses:

authData :: Lens' (Loaded t) (Dynamic t API.AuthData)
authData f loaded' = (\authData' -> loaded' { _authData = authData' }) <$> f (_authData loaded')

families :: Lens' (Loaded t) (Dynamic t (Map API.FamilyId API.Family))
families f loaded' = (\families' -> loaded' { _families = families' }) <$> f (_families loaded')

selectedFamily :: Lens' (Loaded t) (Dynamic t API.FamilyId)
selectedFamily f loaded' = (\selectedFamily' -> loaded' { _selectedFamily = selectedFamily' }) <$> f (_selectedFamily loaded')

-- App lenses:

subscriptions :: Lens' (App t) (SubscriptionsDyn t)
subscriptions f app' = (\subscriptions' -> app' { _subscriptions = subscriptions' }) <$> f (_subscriptions app')

request :: Lens' (App t) (Event t [ API.ServerRequest ])
request f app' = (\request' -> app' { _request = request' }) <$> f (_request app')

selectLang :: Lens' (App t) (Event t Locale)
selectLang f app' = (\selectLang' -> app' { _selectLang = selectLang' }) <$> f (_selectLang app')


-- Screen lenses:

screenApp :: Lens' (Screen t) (App t)
screenApp f screen' = (\screenApp' -> screen' { _screenApp = screenApp' }) <$> f (_screenApp screen')


screenGoHome :: Lens' (Screen t) (Event t ())
screenGoHome f screen' = (\screenGoHome' -> screen' { _screenGoHome = screenGoHome' }) <$> f (_screenGoHome screen')
