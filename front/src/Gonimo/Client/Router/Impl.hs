{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Router.Impl
Description : Routing for Gonimo.
Copyright   : (c) Robert Klotzner, 2018

Picks Impl.Native or Impl.Browser based on `needsNativeHistory` of the Host.
-}
module Gonimo.Client.Router.Impl ( module Gonimo.Client.Router
                                 -- * Types
                                 , Model
                                 , HasModel
                                 , ModelConfig
                                 , HasModelConfig
                                 -- * Creation
                                 , make
                                 ) where

import Reflex.Dom.Core

import           Gonimo.Client.Host                (Host)
import qualified Gonimo.Client.Host                as Host
import           Gonimo.Client.Prelude
import           Gonimo.Client.Router
import qualified Gonimo.Client.Router.Impl.Browser as Browser
import qualified Gonimo.Client.Router.Impl.Native  as Native

-- | Simple data type fulfilling our 'HasModel' constraint.
type Model t = Host t

-- | Our dependencies
type HasModel model = Host.HasHost model

type ModelConfig t = Native.ModelConfig t

type HasModelConfig c t = Native.HasModelConfig c t

make :: forall t m model c mConf
        . (MonadWidget t m , HasModel model, HasConfig c, HasModelConfig mConf t)
      => model t -> c t -> m (mConf t, Router t)
make model conf
  = if model ^. Host.needsNativeHistory
    then Native.make conf
    else (mempty,) <$> Browser.make conf
