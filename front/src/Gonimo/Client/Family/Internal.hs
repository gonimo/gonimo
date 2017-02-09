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
import Gonimo.Client.Server (webSocketConfig_send, webSocket_recv, webSocket_open)

import Gonimo.Client.Subscriber (SubscriptionsDyn)
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Server as Server
import qualified Gonimo.Client.Subscriber as Subscriber


type FamilyMap = Map FamilyId Db.Family

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthData :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateFamily  :: Event t ()
           , _configLeaveFamily  :: Event t ()
           , _configSetName :: Event t Text
           }

data Family t
  = Family { _families :: Dynamic t (Maybe FamilyMap)
           , _selectedFamily :: Dynamic t (Maybe FamilyId)
           , _subscriptions :: SubscriptionsDyn t
           , _request :: Event t [ API.ServerRequest ]
         }

data DefiniteFamily t
  = DefiniteFamily { _definiteFamilies :: Dynamic t FamilyMap
                   , _definiteSelected :: Dynamic t FamilyId
                   }


makeLenses ''Config
makeLenses ''Family
makeLenses ''DefiniteFamily

fromApp :: Reflex t => App.Config t -> Config t
fromApp c = Config { _configResponse = c^.App.server.webSocket_recv
                   , _configAuthData = c^.App.auth^.Auth.authData
                   , _configSelectFamily = never
                   , _configSetName = never
                   , _configAuthenticated = c^.App.auth.Auth.authenticated
                   , _configCreateFamily = never
                   , _configLeaveFamily = never
                   }

-- makeDefinite :: forall m t. Reflex t => Family t -> Dynamic t (Maybe DefiniteFamily t)
-- makeDefinite family' =
--   let
--     onlyJusts = push (pure . id) . updated
--     result = zipDynWith make' (family'^.families) (family'^.selectedFamily)
--     make' mFam mSel = do
--       famInit <- mFam
--       selInit <- mSel
--       pure $ DefiniteFamily { definiteFamilies = hold}

family :: forall m t. (MonadWidget t m) => Config t -> m (Family t)
family config = do
    (subs, familyIds', families') <- makeFamilies config
    -- We need to use ids here, because we need intermediate information not available in families'
    (selectReqs, selected') <- handleFamilySelect config familyIds'
    let createReqs = handleCreateFamily config
    let leaveReqs = handleLeaveFamily config selected'
    let renameReqs = handleSetName config selected'
    pure $ Family { _subscriptions = subs
                  , _families = families'
                  , _selectedFamily = selected'
                  , _request = selectReqs <> createReqs <> leaveReqs <> renameReqs
                }

handleFamilySelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> Dynamic t (Maybe [FamilyId])
                      -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config familyIds' = mdo
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    loadedFamilyId <- GStorage.getItem storage GStorage.currentFamily
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.currentFamily) <$> updated selected

    let switchOnChange = push (\mNewFamilies -> do
                                  mOldFamilies <- sample $ current familyIds'
                                  mcSelected <- sample $ current selected
                                  case (mOldFamilies, mNewFamilies) of
                                    (_, Nothing) -> pure Nothing -- No data, nothing to fix
                                    (Nothing, Just newFams) -> pure $ fixInvalidKey mcSelected newFams
                                    (Just oldFams, Just newFams) -> do
                                      let oldKeys = Set.fromList oldFams
                                      let newKeys = Set.fromList newFams
                                      let createdKey = headMay . fmap Just . Set.toList $ newKeys \\ oldKeys
                                      let fixedIfInvalid = fixInvalidKey mcSelected newFams
                                      pure $ createdKey <|> fixedIfInvalid
                              ) (updated familyIds')
    let
      fixInvalidKey :: Maybe FamilyId -> [FamilyId] -> Maybe (Maybe FamilyId)
      fixInvalidKey Nothing valid = Just <$> headMay valid
      fixInvalidKey (Just key) valid = if key `elem` valid
                                       then Nothing
                                       else Just $ headMay valid

    let selectEvent = leftmost [ Just <$> config^.configSelectFamily, switchOnChange ]
    selected <- holdDyn loadedFamilyId selectEvent
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

handleSetName :: forall t. Reflex t => Config t -> Dynamic t (Maybe FamilyId) -> Event t [ API.ServerRequest ]
handleSetName config selectedFamily' = push (\name -> do
                                                mFid <- sample $ current selectedFamily'
                                                case mFid of
                                                  Nothing -> pure Nothing
                                                  Just fid -> pure $ Just [ API.ReqSetFamilyName fid name ]
                                            ) (config^.configSetName)

makeFamilies :: forall m t. (HasWebView m, MonadWidget t m)
                => Config t -> m (SubscriptionsDyn t, Dynamic t (Maybe [FamilyId]), Dynamic t (Maybe FamilyMap))
makeFamilies config = do
  (subs, fids) <- makeSubscriptions
  families' <- makeFamilyMap fids
  pure (subs, fids, families')
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

     mFamilyIds <- holdDyn Nothing (Just <$> gotFamiliesEvent)
     pure $ ( zipDynWith Set.union familyIdsSubs familiesSubs
            , mFamilyIds
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
