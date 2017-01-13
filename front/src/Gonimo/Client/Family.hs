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
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           }

data Family t
  = Family { _families  :: Dynamic t (Map FamilyId Db.Family)
           , _selectedFamily :: Dynamic t (Maybe FamilyId)
           , _requests :: Event t [ API.ServerRequest ]
           , _subscriptions :: SubscriptionsDyn t
           }

makeLenses ''Config
makeLenses ''Family

family :: forall m t. (HasWebView m, MonadWidget t m) => Config t -> m (Family t)
family config = mdo
  (reqs, selected') <- handleFamilySelect config
  (subs, familyMap) <- makeFamilies config
  pure $ Family { _families = familyMap
                , _selectedFamily = selected'
                , _requests = reqs
                , _subscriptions = subs
                }

handleFamilySelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config = mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    loadedFamilyId <- GStorage.getItem storage GStorage.currentFamily
    selected' <- holdDyn loadedFamilyId $ Just <$> config^.configSelectFamily
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.currentFamily) <$> updated selected'
    reqs <- makeRequests selected'
    pure (reqs, selected')
  where
    makeRequests :: Dynamic t (Maybe FamilyId) -> m (Event t [ API.ServerRequest ])
    makeRequests selected' = do
      let
        mayReq :: Maybe API.AuthData -> Maybe FamilyId -> Maybe API.ServerRequest
        mayReq mAuth mFid = API.ReqSwitchFamily <$> fmap API.deviceId mAuth <*> mFid

        reqDyn = zipDynWith mayReq (config^.configAuthData) selected'
        reqsDyn :: Dynamic t [ API.ServerRequest ]
        reqsDyn = maybe [] (:[]) <$> reqDyn
      pure $ mconcat $ [ tag (current reqsDyn) $ config^.configAuthenticated
                       , updated reqsDyn
                       ]


makeFamilies :: forall m t. (HasWebView m, MonadWidget t m)
                => Config t -> m (SubscriptionsDyn t, Dynamic t (Map FamilyId Db.Family))
makeFamilies config = do
  (,) <$> makeSubscriptions <*> makeFamilyMap
 where
   makeSubscriptions :: m (SubscriptionsDyn t)
   makeSubscriptions = do
     let
       fidsToSubscriptions resp = case resp of
         API.ResGotFamilies _ fids -> pure . Just . foldr Set.insert Set.empty . map API.ReqGetFamily $ fids
         _ -> pure Nothing
       familiesSubsEvent = push fidsToSubscriptions (config^.configResponse)
     familiesSubs <- holdDyn Set.empty familiesSubsEvent

     let familyIdsSubs = maybe Set.empty (Set.singleton . API.ReqGetFamilies . API.accountId)
                          <$> config^.configAuthData

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

       familyMapEv = push resToFamily $ config^.configResponse
     familyMap <- holdDyn Map.empty familyMapEv
     pure familyMap
