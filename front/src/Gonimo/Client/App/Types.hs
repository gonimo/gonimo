{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.App.Types where

import Reflex
import qualified Gonimo.SocketAPI as API
import Control.Lens
import qualified Gonimo.Client.Server as Server
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Subscriber as Subscriber
import Gonimo.Client.Subscriber (SubscriptionsDyn)

data Config t
  = Config { _server :: Server.Server t
           , _auth :: Auth.Auth t
           , _subscriber :: Subscriber.Subscriber t
           }

data App t
  = App { _subscriptions :: SubscriptionsDyn t
        , _request :: Event t [ API.ServerRequest ]
        }

makeLenses ''Config
makeLenses ''App
