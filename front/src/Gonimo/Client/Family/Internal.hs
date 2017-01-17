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

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateFamily  :: Event t ()
           , _configLeaveFamily  :: Event t ()
           }

data Init t
  = Init { _initSubscriptions :: SubscriptionsDyn t
         , _initFamily :: forall m. (MonadWidget t m) => Event t (Config t -> m (Family t))
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
  let leaveReqs = handleLeaveFamily config selected'
  (selectReqs, selected') <- handleFamilySelect config families'
  pure $ Family { _families = families'
                , _selectedFamily = selected'
                , _request = selectReqs <> createReqs <> leaveReqs
                }


handleFamilySelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> Dynamic t (Map FamilyId Db.Family)
                      -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config families' = mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    loadedFamilyId <- GStorage.getItem storage GStorage.currentFamily
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.currentFamily) <$> updated selected

    let switchOnChange = push (\newFamilies -> do
                                  oldFamilies <- sample $ current families'
                                  let oldKeys = Set.fromList $ Map.keys oldFamilies
                                  let newKeys = Set.fromList $ Map.keys newFamilies
                                  let newKey = headMay . Set.toList $ newKeys \\ oldKeys
                                  let deletedKeys = oldKeys \\ newKeys
                                  fixedIfInvalid <- runMaybeT $ do
                                    currentSelection <- MaybeT . sample $ current selected
                                    if Set.member currentSelection deletedKeys
                                      then MaybeT . pure . headMay $ Set.toList newKeys
                                      else pure currentSelection
                                  pure $ newKey <|> fixedIfInvalid
                              ) (updated families')
    let selectEvent = leftmost [ config^.configSelectFamily, switchOnChange ]
    selected <- holdDyn loadedFamilyId $ Just <$> selectEvent

    reqs <- makeRequest selected

    pure (reqs, selected)
  where
    makeRequest :: Dynamic t (Maybe FamilyId) -> m (Event t [ API.ServerRequest ])
    makeRequest selected' = do
      let
        mayReq :: Maybe API.AuthData -> Maybe FamilyId -> Maybe API.ServerRequest
        mayReq mAuth mFid = API.ReqSwitchFamily <$> fmap API.deviceId mAuth <*> mFid

        reqDyn = zipDynWith mayReq (config^.configAuthData) selected'
        reqsDyn :: Dynamic t [ API.ServerRequest ]
        reqsDyn = maybe [] (:[]) <$> reqDyn

      (initEv, makeInitEv) <- newTriggerEvent
      liftIO $ makeInitEv ()
      pure $ mconcat $ [ tag (current reqsDyn) $ leftmost [ config^.configAuthenticated, initEv ]
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
                => Config t -> m (SubscriptionsDyn t, Event t (Dynamic t (Map FamilyId Db.Family)))
makeFamilies config = do
  (subs, fids) <- makeSubscriptions
  familiesEv <- waitForReady =<< makeFamilyMap fids
  pure (subs, familiesEv)
 where
   makeSubscriptions :: m (SubscriptionsDyn t, Event t [FamilyId])
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
            , gotFamiliesEvent
            )

   makeFamilyMap :: Event t [FamilyId] -> m (Event t (Map FamilyId Db.Family))
   makeFamilyMap allFids = mdo
     let
       resToFamily :: API.ServerResponse -> PushM t (Maybe (Map FamilyId Db.Family))
       resToFamily resp = case resp of
         API.ResGotFamily fid family' -> do
           oldMap <- sample $ current familyMap
           pure $ Just (oldMap & at fid .~ Just family')
         _                           -> pure Nothing

       makeConsistent :: Map FamilyId Db.Family -> [FamilyId] -> Maybe (Map FamilyId Db.Family)
       makeConsistent families' fids =
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
     allFidsReady :: Event t (Dynamic t [FamilyId]) <- waitForReady allFids
     switchPromptly never $
       push (pure . id) . updated . zipDynWith makeConsistent familyMap <$> allFidsReady
