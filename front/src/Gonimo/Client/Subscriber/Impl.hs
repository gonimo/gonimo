{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Gonimo.Client.Subscriber.Impl ( module Gonimo.Client.Subscriber
                                , Model (..)
                                , HasModel
                                , _server
                                , _auth
                                , make
                                , SubscriptionsDyn
                                , subscribeKeys
                                )where

import           Control.Monad.Fix            (MonadFix)
import           Reflex.Dom.Core
import           Data.Map                     (Map)

import           Control.Lens

import qualified Data.Set                     as Set

import           Gonimo.Client.Auth       (Auth)
import qualified Gonimo.Client.Auth       as Auth
import           Gonimo.Client.Model
import           Gonimo.Client.Reflex         (buildMap)
import           Gonimo.Client.Server         (Server)
import qualified Gonimo.Client.Server         as Server
import           Gonimo.Client.Subscriber
import qualified Gonimo.SocketAPI             as API
import           Reflex.Class.Extended

data Model t
  = Model { __server :: Server t
          , __auth   :: Auth t
          }


-- | Constraint on needed dependencies.
type HasModel model = (Server.HasServer model, Auth.HasAuth model)

-- | Constraint for our return value.
type HasModelConfig c t = (IsConfig c t, Server.HasConfig c)


make :: forall c model m t mConf
        . ( MonadFix m, MonadHold t m, Reflex t
          , HasConfig c , HasModel model, HasModelConfig mConf t
          )
     => model t -> c t -> m (mConf t)
make model conf = do
  let
    requests = API.ReqSetSubscriptions . Set.toList <$> conf^.subscriptions

  pure $ mempty  & Server.onRequest .~
    mergeAsList [ tag (current requests) $ model^.Auth.onAuthenticated
                , updated requests
                ]

subscribeKeys :: forall m t key val . (MonadFix m, Reflex t, MonadHold t m, Ord key)
                 => Dynamic t [key] -> (key -> API.ServerRequest) -> Event t (key, val)
              -> m (SubscriptionsDyn t, Dynamic t (Map key (Dynamic t val)))
subscribeKeys keys mkKeyRequest gotNewKeyVal = do
    let getValsSubs = Set.unions . map (Set.singleton . mkKeyRequest) <$> keys
    resultMap <- buildMap keys gotNewKeyVal
    pure (getValsSubs, resultMap)

-- Auto generated lenses:

-- Lenses for Model t:

_server :: Lens' (Model t) (Server t)
_server f fullConfig' = (\_server' -> fullConfig' { __server = _server' }) <$> f (__server fullConfig')

_auth :: Lens' (Model t) (Auth t)
_auth f fullConfig' = (\_auth' -> fullConfig' { __auth = _auth' }) <$> f (__auth fullConfig')


