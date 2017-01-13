{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Subscriber where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Config t
  = Config { _configSubscriptions :: Dynamic t (Set API.ServerRequest)
           , _configResponse :: Event t API.ServerResponse -- push subscriptions on Authenticated
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

    authenticated :: Event t ()
    authenticated = do
      let handleAuthenticated resp = pure $ case resp of
            API.ResAuthenticated -> Just ()
            _                    -> Nothing
      push handleAuthenticated $ config^.configResponse


  pure $ Subscriber { _request = mconcat
                      . map (fmap (:[]))
                      $ [ tag (current requests) $ authenticated
                        , updated requests
                        ]
                    }

