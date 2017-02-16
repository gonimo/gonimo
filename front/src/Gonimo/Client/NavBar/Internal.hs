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


data Config t
  = Config { _configLoaded :: App.Loaded t
           , _configDeviceName :: Dynamic t Text
           }

data NavBar t
  = NavBar { _backClicked :: Event t ()
           , _homeClicked :: Event t ()
           }


makeLenses ''Config
makeLenses ''NavBar
