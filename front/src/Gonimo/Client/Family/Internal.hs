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
import Data.Set (Set, (\\))
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative

type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)
type FamilyMap = Map FamilyId Db.Family

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateFamily  :: Event t ()
           , _configLeaveFamily  :: Event t ()
           }

data Family t
  = Family { _families :: Dynamic t (Maybe FamilyMap)
           , _selectedFamily :: Dynamic t (Maybe FamilyId)
           , _subscriptions :: SubscriptionsDyn t
           , _request :: Event t [ API.ServerRequest ]
         }

makeLenses ''Config
makeLenses ''Family

family :: forall m t. (MonadWidget t m) => Config t -> m (Family t)
family config = do
    (subs, families') <- makeFamilies config
    (selectReqs, selected') <- handleFamilySelect config families'
    let createReqs = handleCreateFamily config
    let leaveReqs = handleLeaveFamily config selected'
    pure $ Family { _subscriptions = subs
                  , _families = families'
                  , _selectedFamily = selected'
                  , _request = selectReqs <> createReqs <> leaveReqs
                }

handleFamilySelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> Dynamic t (Maybe FamilyMap)
                      -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config families' = mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    loadedFamilyId <- GStorage.getItem storage GStorage.currentFamily
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.currentFamily) <$> updated selected

    let switchOnChange = push (\mNewFamilies -> do
                                  mOldFamilies <- sample $ current families'
                                  mcSelected <- sample $ current selected
                                  case (mOldFamilies, mNewFamilies) of
                                    (_, Nothing) -> pure Nothing -- No data, nothing to fix
                                    (Nothing, Just newFams) -> pure $ fixInvalidKey mcSelected (Map.keys newFams)
                                    (Just oldFams, Just newFams) -> do
                                      let oldKeys = Set.fromList $ Map.keys oldFams
                                      let newKeys = Set.fromList $ Map.keys newFams
                                      let createdKey = headMay . fmap Just . Set.toList $ newKeys \\ oldKeys
                                      let fixedIfInvalid = fixInvalidKey mcSelected (Map.keys newFams)
                                      pure $ createdKey <|> fixedIfInvalid
                              ) (updated families')
    let
      fixInvalidKey :: Maybe FamilyId -> [FamilyId] -> Maybe (Maybe FamilyId)
      fixInvalidKey Nothing valid = Just <$> headMay valid
      fixInvalidKey (Just key) valid = if key `elem` valid
                                       then Nothing
                                       else Just $ headMay valid

    let selectEvent = leftmost [ Just <$> config^.configSelectFamily, switchOnChange ]
    selected <- holdDyn loadedFamilyId selectEvent
    reqs <- makeRequest selected

    pure (reqs, zipDynWith makeSelectionConsistent families' selected)
  where
    makeSelectionConsistent :: Maybe FamilyMap -> Maybe FamilyId -> Maybe FamilyId
    makeSelectionConsistent mFamily mFid = mFamily *> mFid

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

handleCreateFamily :: forall t. Reflex t => Config t -> Event t [ API.ServerRequest ]
handleCreateFamily config = const [API.ReqCreateFamily] <$> config^.configCreateFamily

handleLeaveFamily :: forall t. Reflex t => Config t -> Dynamic t (Maybe FamilyId) -> Event t [ API.ServerRequest ]
handleLeaveFamily config selected'
  = push (\_ -> runMaybeT $ do
             auth <- MaybeT . sample $ current (config^.configAuthData)
             selected <- MaybeT . sample $ current selected'
             let req = API.ReqLeaveFamily (API.accountId auth) selected
             pure [req]
         ) (config^.configLeaveFamily)


makeFamilies :: forall m t. (HasWebView m, MonadWidget t m)
                => Config t -> m (SubscriptionsDyn t, Dynamic t (Maybe FamilyMap))
makeFamilies config = do
  (subs, fids) <- makeSubscriptions
  families' <- makeFamilyMap fids
  pure (subs, families')
 where
   makeSubscriptions :: m (SubscriptionsDyn t, Dynamic t (Maybe [FamilyId]))
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

     familyIds <- holdDyn Nothing (Just <$> gotFamiliesEvent)
     pure $ (zipDynWith Set.union familyIdsSubs familiesSubs
            , familyIds
            )

  -- Nothing until consistent.
   makeFamilyMap :: Dynamic t (Maybe [FamilyId]) -> m (Dynamic t (Maybe FamilyMap))
   makeFamilyMap allFids = mdo
     let
       resToFamily :: API.ServerResponse -> PushM t (Maybe FamilyMap)
       resToFamily resp = case resp of
         API.ResGotFamily fid family' -> do
           oldMap <- sample $ current familyMap
           pure $ Just (oldMap & at fid .~ Just family')
         _                           -> pure Nothing

       makeConsistent :: FamilyMap -> Maybe [FamilyId] -> Maybe FamilyMap
       makeConsistent _ Nothing = Nothing
       makeConsistent families' (Just fids) =
         let
           oldFids = Set.fromList $ Map.keys families'
           newFids = Set.fromList fids
           deletedFids = oldFids \\ newFids
           createdFids = newFids \\ oldFids
           fixedMap = foldr Map.delete families' deletedFids
         in
           if Set.null createdFids
           then Just fixedMap
           else Nothing -- Map not yet complete

       familyMapEv = push resToFamily $ config^.configResponse
     familyMap <- holdDyn Map.empty familyMapEv
     pure $ zipDynWith makeConsistent familyMap allFids
