{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.NavBar.Internal where

import Reflex.Dom.Core
import Control.Lens
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.DeviceList.Internal as DeviceList
import qualified Gonimo.SocketAPI as API

data Config t
  = Config { _configLoaded     :: App.Loaded t
           , _configDeviceList :: DeviceList.DeviceList t
           }

data NavBar t
  = NavBar { _backClicked :: Event t ()
           , _homeClicked :: Event t ()
           , _request     :: Event t [API.ServerRequest]
           }


-- Lenses for Config t:

configLoaded :: Lens' (Config t) (App.Loaded t)
configLoaded f config' = (\configLoaded' -> config' { _configLoaded = configLoaded' }) <$> f (_configLoaded config')

configDeviceList :: Lens' (Config t) (DeviceList.DeviceList t)
configDeviceList f config' = (\configDeviceList' -> config' { _configDeviceList = configDeviceList' }) <$> f (_configDeviceList config')


-- Lenses for NavBar t:

backClicked :: Lens' (NavBar t) (Event t ())
backClicked f navBar' = (\backClicked' -> navBar' { _backClicked = backClicked' }) <$> f (_backClicked navBar')

homeClicked :: Lens' (NavBar t) (Event t ())
homeClicked f navBar' = (\homeClicked' -> navBar' { _homeClicked = homeClicked' }) <$> f (_homeClicked navBar')

request :: Lens' (NavBar t) (Event t [API.ServerRequest])
request f navBar' = (\request' -> navBar' { _request = request' }) <$> f (_request navBar')


