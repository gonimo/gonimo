{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.Subscriber where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens

import Gonimo.Client.Reflex (buildMap)

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Config t
  = Config { _configSubscriptions :: Dynamic t (Set API.ServerRequest)
           , _configResponse :: Event t API.ServerResponse -- push subscriptions on Authenticated
           , _configAuthenticated :: Event t ()
           }

data Subscriber t
  = Subscriber { _request :: Event t [ API.ServerRequest ]
               }

makeLenses ''Config
makeLenses ''Subscriber

subscriber :: forall m t. (HasWebView m, MonadWidget t m)
              => Config t -> m (Subscriber t)
subscriber config = do
  let
    requests = API.ReqSetSubscriptions . Set.toList <$> config^.configSubscriptions
  pure $ Subscriber { _request = mconcat
                      . map (fmap (:[]))
                      $ [ tag (current requests) $ config^.configAuthenticated
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
