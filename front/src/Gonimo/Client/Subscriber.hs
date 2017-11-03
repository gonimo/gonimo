{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.Subscriber where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)

import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

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

-- Lenses for Config t:

configSubscriptions :: Lens' (Config t) (Dynamic t (Set API.ServerRequest))
configSubscriptions f config' = (\configSubscriptions' -> config' { _configSubscriptions = configSubscriptions' }) <$> f (_configSubscriptions config')

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthenticated :: Lens' (Config t) (Event t ())
configAuthenticated f config' = (\configAuthenticated' -> config' { _configAuthenticated = configAuthenticated' }) <$> f (_configAuthenticated config')


-- Lenses for Subscriber t:

request :: Lens' (Subscriber t) (Event t [ API.ServerRequest ])
request f subscriber' = (\request' -> subscriber' { _request = request' }) <$> f (_request subscriber')


