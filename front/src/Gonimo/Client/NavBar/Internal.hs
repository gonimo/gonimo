{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.NavBar.Internal where

import Reflex.Dom.Core
import Control.Lens
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.DeviceList.Internal as DeviceList

-- data Wrapper needed (instead of Maybe) because:
-- GHC doesn't yet support impredicative polymorphism
data ConfirmationText t
  = NoConfirmation
  | WithConfirmation (forall m. (HasWebView m, MonadWidget t m) => m ())

data Config t
  = Config { _configLoaded :: App.Loaded t
           , _configDeviceList :: DeviceList.DeviceList t
           , _configConfirmationOnBack :: ConfirmationText t
           , _configConfirmationOnHome :: ConfirmationText t
           }

data NavBar t
  = NavBar { _backClicked :: Event t ()
           , _homeClicked :: Event t ()
           }


makeLenses ''Config
makeLenses ''NavBar
