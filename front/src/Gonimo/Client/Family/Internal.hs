{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.Family.Internal where

import Reflex.Dom
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
import Data.Maybe (isJust, isNothing)
import Safe (headMay)
import Data.List (sort)
import Gonimo.Client.Reflex

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateFamily  :: Event t ()
           }

data Init t
  = Init { _initSubscriptions :: SubscriptionsDyn t
         , _initFamily :: forall m. (HasWebView m, MonadWidget t m) => Event t (Config t -> m (Family t))
         }

data Family t
  = Family { _families :: Dynamic t (Map FamilyId Db.Family)
           , _selectedFamily :: Dynamic t (Maybe FamilyId)
           , _request :: Event t [ API.ServerRequest ]

           }

makeLenses ''Config
makeLenses ''Init
makeLenses ''Family

init :: forall m t. (MonadWidget t m) => Config t -> m (Init t)
init config = do
    (subs, familiesReady) <- makeFamilies config
    pure $ Init { _initSubscriptions = subs
                , _initFamily = flip family <$> familiesReady
                }

-- | Not to be used directly - call init.
family :: forall m t. (HasWebView m, MonadWidget t m)
          => Config t -> Dynamic t (Map FamilyId Db.Family) -> m (Family t)
family config families' = mdo
  let createReqs = handleCreateFamily config
  (selectReqs, selected') <- handleFamilySelect config families'
  pure $ Family { _families = families'
                , _selectedFamily = selected'
                , _request = selectReqs <> createReqs
                }


handleFamilySelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> Dynamic t (Map FamilyId Db.Family)
                      -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config families' = mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    loadedFamilyId <- GStorage.getItem storage GStorage.currentFamily
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.currentFamily) <$> updated selectedChecked

    selectedUnchecked <- holdDyn loadedFamilyId $ Just <$> config^.configSelectFamily
    let selectedChecked = fixSelected selectedUnchecked

    reqs <- makeRequest selectedChecked

    pure (reqs, selectedChecked)
  where
    makeRequest :: Dynamic t (Maybe FamilyId) -> m (Event t [ API.ServerRequest ])
    makeRequest selected' = do
      let
        mayReq :: Maybe API.AuthData -> Maybe FamilyId -> Maybe API.ServerRequest
        mayReq mAuth mFid = API.ReqSwitchFamily <$> fmap API.deviceId mAuth <*> mFid

        reqDyn = zipDynWith mayReq (config^.configAuthData) selected'
        reqsDyn :: Dynamic t [ API.ServerRequest ]
        reqsDyn = maybe [] (:[]) <$> reqDyn
      pure $ mconcat $ [ tag (current reqsDyn) $ config^.configAuthenticated
                       , updated reqsDyn
                       ]
    fixSelected :: Dynamic t (Maybe FamilyId) -> Dynamic t (Maybe FamilyId)
    fixSelected selected' =
      let
        hasSelection selected'' families''
          = case selected'' of
              Nothing -> False
              Just s  -> isJust (families'' ^. at s)

        fixSelection :: Maybe FamilyId -> Map FamilyId Db.Family -> Maybe FamilyId
        fixSelection selected'' families''
          = if hasSelection selected'' families''
            then selected''
            else headMay . Map.keys $ families''
      in
        zipDynWith fixSelection selected' families'

handleCreateFamily :: forall t. Reflex t => Config t -> Event t [ API.ServerRequest ]
handleCreateFamily config = const [API.ReqCreateFamily] <$> config^.configCreateFamily


makeFamilies :: forall m t. (HasWebView m, MonadWidget t m)
                => Config t -> m (SubscriptionsDyn t, Event t (Dynamic t (Map FamilyId Db.Family)))
makeFamilies config = do
  (subs, fids) <- makeSubscriptions
  mFamilies <- makeFamilyMap fids
  pure (subs, waitForReady mFamilies)
 where
   makeSubscriptions :: m (SubscriptionsDyn t, Dynamic t [FamilyId])
   makeSubscriptions = do
     let
       handleGotFamilies resp = case resp of
         API.ResGotFamilies _ fids -> pure . Just $ fids
         _ -> pure Nothing
       gotFamiliesEvent = push handleGotFamilies (config^.configResponse)

       fidsToSubscriptions = foldr Set.insert Set.empty . map API.ReqGetFamily

     familyIds <- holdDyn [] gotFamiliesEvent
     let familiesSubs = fidsToSubscriptions <$> familyIds

     let familyIdsSubs = maybe Set.empty (Set.singleton . API.ReqGetFamilies . API.accountId)
                          <$> config^.configAuthData

     pure $ (zipDynWith Set.union familyIdsSubs familiesSubs
            , familyIds
            )

   makeFamilyMap :: Dynamic t [FamilyId] -> m (Dynamic t (Maybe (Map FamilyId Db.Family)))
   makeFamilyMap allFids = mdo
     let
       resToFamily :: API.ServerResponse -> PushM t (Maybe (Map FamilyId Db.Family))
       resToFamily resp = case resp of
         API.ResGotFamily fid family -> do
           oldMap <- sample $ current familyMap
           pure $ Just (Map.insert fid family oldMap)
         _                           -> pure Nothing

       completeOrNothing :: [FamilyId] -> Map FamilyId Db.Family -> Maybe (Map FamilyId Db.Family)
       completeOrNothing fids families' = if sort (Map.keys families') == sort fids
                                          then Just families'
                                          else Nothing

       familyMapEv = push resToFamily $ config^.configResponse
     familyMap <- holdDyn Map.empty familyMapEv
     pure $ zipDynWith completeOrNothing allFids familyMap
