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
import qualified Gonimo.SocketAPI as API

data Config t
  = Config { _configLoaded :: App.Loaded t
           , _configDeviceList :: DeviceList.DeviceList t
           }

data NavBar t
  = NavBar { _backClicked :: Event t ()
           , _homeClicked :: Event t ()
           , _request :: Event t [API.ServerRequest]
           }


makeLenses ''Config
makeLenses ''NavBar
