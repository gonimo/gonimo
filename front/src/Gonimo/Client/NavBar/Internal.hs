{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.NavBar.Internal where

import Reflex.Dom
import Control.Lens
import Data.Text (Text)
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.DeviceList.Internal as DeviceList


data Config t
  = Config { _configLoaded :: App.Loaded t
           , _configDeviceList :: DeviceList.DeviceList t
           }

data NavBar t
  = NavBar { _backClicked :: Event t ()
           , _homeClicked :: Event t ()
           }


makeLenses ''Config
makeLenses ''NavBar
