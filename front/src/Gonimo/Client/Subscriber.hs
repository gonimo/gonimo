{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.Subscriber ( module Gonimo.Client.Subscriber.API
                                , Deps (..)
                                , HasDeps
                                , _server
                                , _auth
                                , make
                                , SubscriptionsDyn
                                , subscribeKeys
                                )where

import           Control.Monad.Fix      (MonadFix)
import           Reflex.Dom.Core

import           Data.Map               (Map)

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Gonimo.Client.Auth.API (Auth)
import qualified Gonimo.Client.Auth.API as Auth
import           Gonimo.Client.Server   (Server)
import qualified Gonimo.Client.Server   as Server
import qualified Gonimo.SocketAPI       as API


import           Control.Lens

import           Gonimo.Client.Reflex   (buildMap)
import           Gonimo.Client.Subscriber.API

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Deps t
  = Deps { __server :: Server t
         , __auth   :: Auth t
         }


-- | We simply provide configuration for Server.
type FullSubscriber t = Server.Config t

-- | Constraint on needed dependencies.
type HasDeps d = (Server.HasServer d, Auth.HasAuth d)

make :: forall c d m t. ( HasWebView m, MonadWidget t m, HasConfig c
                          , HasDeps d)
           => d t -> c t -> m (FullSubscriber t)
make deps conf = do
  let
    requests = API.ReqSetSubscriptions . Set.toList <$> conf^.subscriptions

  pure $ Server.Config { Server._onRequest = mconcat
                          . map (fmap (:[]))
                          $ [ tag (current requests) $ deps^.Auth.onAuthenticated
                            , updated requests
                            ]
                        }

subscribeKeys :: forall m t key val . (MonadFix m, Reflex t, MonadHold t m, Ord key)
                 => Dynamic t [key] -> (key -> API.ServerRequest) -> Event t (key, val)
              -> m (SubscriptionsDyn t, Dynamic t (Map key (Dynamic t val)))
subscribeKeys keys mkKeyRequest gotNewKeyVal = do
    let getValsSubs = Set.unions . map (Set.singleton . mkKeyRequest) <$> keys
    resultMap <- buildMap keys gotNewKeyVal
    pure (getValsSubs, resultMap)

-- Auto generated lenses:

-- Lenses for Deps t:

_server :: Lens' (Deps t) (Server t)
_server f fullConfig' = (\_server' -> fullConfig' { __server = _server' }) <$> f (__server fullConfig')

_auth :: Lens' (Deps t) (Auth t)
_auth f fullConfig' = (\_auth' -> fullConfig' { __auth = _auth' }) <$> f (__auth fullConfig')


