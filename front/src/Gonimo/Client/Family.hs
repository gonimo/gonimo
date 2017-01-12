{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Family where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data FamilyConfig t
  = FamilyConfig { _familyConfigResponse :: Event t API.ServerResponse
                 , _familyConfigAuthData :: Dynamic t (Maybe API.AuthData)
                 }

data Family t
  = Family { _familyFamilies  :: Dynamic t (Map FamilyId Db.Family)
           , _familyCurrentId :: Dynamic t (Maybe FamilyId)
           , _familyRequests :: Event t [ API.ServerRequest ]
           , _familySubscriptions :: SubscriptionsDyn t
           }

makeLenses ''FamilyConfig
makeLenses ''Family

makeFamilies :: forall m t. (HasWebView m, MonadWidget t m)
  => FamilyConfig t -> m (SubscriptionsDyn t, Dynamic t (Map FamilyId Db.Family))
makeFamilies config = do
  (,) <$> makeSubscriptions <*> makeFamilyMap
 where
   makeSubscriptions :: m (SubscriptionsDyn t)
   makeSubscriptions = do
     let
       fidsToSubscriptions resp = case resp of
         API.ResGotFamilies _ fids -> pure . Just . foldr Set.insert Set.empty . map API.ReqGetFamily $ fids
         _ -> pure Nothing
       familiesSubsEvent = push fidsToSubscriptions (config^.familyConfigResponse)
     familiesSubs <- holdDyn Set.empty familiesSubsEvent

     let familyIdsSubs = maybe Set.empty (Set.singleton . API.ReqGetFamilies . API.accountId)
                          <$> config^.familyConfigAuthData

     pure $ zipDynWith Set.union familyIdsSubs familiesSubs

   makeFamilyMap :: m (Dynamic t (Map FamilyId Db.Family))
   makeFamilyMap = mdo
     let
       resToFamily :: API.ServerResponse -> PushM t (Maybe (Map FamilyId Db.Family))
       resToFamily resp = case resp of
         API.ResGotFamily fid family -> do
           oldMap <- sample $ current familyMap
           pure $ Just (Map.insert fid family oldMap)
         _                           -> pure Nothing

       familyMapEv = push resToFamily $ config^.familyConfigResponse
     familyMap <- holdDyn Map.empty familyMapEv
     pure familyMap
