{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.Subscriber where

import Reflex.Dom
import Control.Monad.Fix (MonadFix)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens

import Gonimo.Client.Reflex (waitForReady)

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
subscribeKeys keys mkKeyRequest gotNewKeyVal = mdo
  let
    gotNewVal :: key -> Event t val
    gotNewVal key' = push (\(k,v)
                           -> pure $ if k == key'
                                     then Just v
                                     else Nothing
                          ) gotNewKeyVal

    getValsSubs = Set.unions . map (Set.singleton . mkKeyRequest) <$> keys

    insertKeys :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val))
    insertKeys = push (\(key, val) -> do
                          oldMap <- sample $ current resultMap
                          if (Map.member key oldMap)
                          then
                            pure Nothing -- Nothing to do.
                          else do
                            dynVal <- holdDyn val (gotNewVal key)
                            pure . Just $ Map.insert key dynVal
                      ) gotNewKeyVal

    deleteKeys :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val))
    deleteKeys = push (\keys' -> do
                         oldKeys <- fmap Set.fromList . sample $ current keys
                         let newKeys = Set.fromList keys'
                         let deletedKeys = oldKeys \\ newKeys
                         if Set.null deletedKeys
                           then pure Nothing
                           else pure $ Just $ \oldMap -> foldr Map.delete oldMap deletedKeys
                     ) (updated keys)

    updateMap :: Event t (Map key (Dynamic t val) -> Map key (Dynamic t val)) -> Event t (Map key (Dynamic t val))
    updateMap = push (\updateF -> do
                         oldMap <- sample $ current resultMap
                         pure $ Just $ updateF oldMap
                     )
  resultMap <- holdDyn Map.empty . updateMap $ mergeWith (.) [ insertKeys, deleteKeys ]
  pure (getValsSubs, resultMap)
