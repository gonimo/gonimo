module Gonimo.Client.App.Types where

import           Control.Lens
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Set                 as Set

import           Gonimo.Client.Account    (Account, HasAccount)
import qualified Gonimo.Client.Account    as Account
import           Gonimo.Client.Auth       (Auth, HasAuth)
import qualified Gonimo.Client.Auth       as Auth
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server     (HasServer, Server)
import qualified Gonimo.Client.Server     as Server
import           Gonimo.Client.Subscriber (SubscriptionsDyn)
import           Gonimo.I18N
import qualified Gonimo.SocketAPI         as API
import qualified Gonimo.SocketAPI.Types   as API
import qualified Gonimo.Types             as Gonimo



data Config t
  = Config { __server  :: Server t
           , __account :: Account t
           , __auth    :: Auth t
            -- | Is also in the Reader environment for translation convenience.
           , _gonimoLocale :: Dynamic t Locale
           }

data Loaded t
  = Loaded { _authData       :: Dynamic t API.AuthData
           , _families       :: Dynamic t (Map API.FamilyId API.Family)
           , _selectedFamily :: Dynamic t API.FamilyId
           }

data App t
  = App { _subscriptions :: SubscriptionsDyn t
          -- TODO: Should be Server.Config
        , _request       :: Event t [ API.ServerRequest ]
        , _selectLang    :: Event t Locale
        }

data Screen t
  = Screen { _screenApp    :: App t
           , _screenGoHome :: Event t ()
           }

instance HasServer Config where
  server = _server

instance HasAccount Config where
  account = _account

instance HasAuth Config where
  auth = _auth

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


-- Auto generated lenses:

-- Lenses for Config t:

_server :: Lens' (Config t) (Server t)
_server f config' = (\_server' -> config' { __server = _server' }) <$> f (__server config')

_account :: Lens' (Config t) (Account t)
_account f config' = (\_account' -> config' { __account = _account' }) <$> f (__account config')

_auth :: Lens' (Config t) (Auth t)
_auth f config' = (\_auth' -> config' { __auth = _auth' }) <$> f (__auth config')

gonimoLocale :: Lens' (Config t) (Dynamic t Locale)
gonimoLocale f config' = (\gonimoLocale' -> config' { _gonimoLocale = gonimoLocale' }) <$> f (_gonimoLocale config')



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
