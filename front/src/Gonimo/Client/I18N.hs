module Gonimo.Client.I18N where

import Gonimo.I18N
import           Reflex.Dom.Core
import Control.Monad.Reader.Class


newtype GonimoEnv t = GonimoEnv { _gonimoLocale :: Dynamic t Locale
                                }


-- makeLenses ''GonimoEnv

trText :: (DomBuilder t m, PostBuild t m, I18N msg, MonadReader (GonimoEnv t) m)
          => msg -> m ()
trText = trDynText . pure

trDynText :: (DomBuilder t m, PostBuild t m, I18N msg, MonadReader (GonimoEnv t) m)
          => Dynamic t msg -> m ()
trDynText msg = do
  loc <- asks _gonimoLocale
  dynText $ i18n <$> loc <*> msg
