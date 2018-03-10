{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Settings
Description : Manage user settings.
Copyright   : (c) Robert Klotzner, 2018
This is about settings that can be manipulated by the user. Like the language to
use or the preferred camera.

For settings coming from the environment like the server address, please have a
look at "Gonimo.Client.Environment".
-}
module Gonimo.Client.Settings ( -- * Types and Classes
                            Config (..)
                          , HasConfig (..)
                          , Settings (..)
                          , HasSettings (..)
                            -- * Creation
                          , make
                            -- * Translated text rendering in a Reader Monad
                          , trText
                          , trDynText
                          )where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Data.Default
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                   (Text)
import           GHC.Generics
import qualified GHCJS.DOM                   as DOM
import           GHCJS.DOM.Types             (MonadJSM, liftJSM)
import qualified GHCJS.DOM.Window            as Window
import qualified Gonimo.Client.Storage       as GStorage
import qualified Gonimo.Client.Storage.Keys  as GStorage
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Class.Extended
import           Reflex.Dom.Core
import qualified Data.Text                   as T

import           Gonimo.I18N

-- | Example Config satisfying 'HasConfig'.
--
--   Requests for setting changes can be passed to 'make' by using 'Config' or any other data type satisfying 'HasConfig'.
newtype Config t
  = Config { -- | Select a different locale/language.
             _onSelectLocale :: Event t Locale
           } deriving (Generic)


-- | User settings, like selected language.
newtype Settings t = Settings { _locale :: Dynamic t Locale
                              }

make :: (Reflex t, MonadHold t m, MonadFix m, MonadJSM m, PerformEvent t m, MonadJSM (Performable m),  HasConfig c)
     => c t -> m (Settings t)
make conf = do
  initLang      <- readLocale
  _locale <- holdDyn initLang $ conf^.onSelectLocale
  performEvent_ $ writeLocale <$> updated _locale
  pure Settings{..}


-- | Render a text in the current locale.
trText :: (DomBuilder t m, PostBuild t m, I18N msg, HasSettings s, MonadReader (s t) m)
          => msg -> m ()
trText = trDynText . pure

-- | Render dynamic text in the current locale.
trDynText :: (DomBuilder t m, PostBuild t m, I18N msg, HasSettings s, MonadReader (s t) m)
          => Dynamic t msg -> m ()
trDynText msg = do
  loc <- view locale
  dynText $ i18n <$> loc <*> msg

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

-- | Parse the browser string locale information.
localeFromBrowserString :: Text -> Locale
localeFromBrowserString langStr
  | T.isPrefixOf "de" langStr = DE_DE
  | otherwise = EN_GB


instance Reflex t => Semigroup (Config t) where
  c1 <> c2 = Config $ _onSelectLocale c1 <> _onSelectLocale c2

instance Reflex t => Monoid (Config t) where
  mempty = Config never
  mappend = (<>)

instance Reflex t => Default (Config t) where
  def = mempty

instance Flattenable Config where
  flattenWith doSwitch ev = Config <$> doSwitch never (_onSelectLocale <$> ev)


-- Auto generated lenses:

class HasConfig a where
  config :: Lens' (a t) (Config t)

  onSelectLocale :: Lens' (a t) (Event t Locale)
  onSelectLocale = config . go
    where
      go :: Lens' (Config t) (Event t Locale)
      go f config' = (\onSelectLocale' -> config' { _onSelectLocale = onSelectLocale' }) <$> f (_onSelectLocale config')


instance HasConfig Config where
  config = id


class HasSettings a where
  settings :: Lens' (a t) (Settings t)

  locale :: Lens' (a t) (Dynamic t Locale)
  locale = settings . go
    where
      go :: Lens' (Settings t) (Dynamic t Locale)
      go f settings' = (\locale' -> settings' { _locale = locale' }) <$> f (_locale settings')


instance HasSettings Settings where
  settings = id
