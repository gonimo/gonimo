{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Family.Internal where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Default               (Default (..))
import           Data.Foldable              (traverse_)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   ((\\))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified GHCJS.DOM                  as DOM
import qualified GHCJS.DOM.Window           as Window
import           Reflex.Dom.Core
import           Safe                       (headMay)

import qualified Gonimo.Client.App.Types    as App
import qualified Gonimo.Client.Auth.Impl    as Auth
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server       hiding (Config)
import qualified Gonimo.Client.Storage      as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import           Gonimo.Client.Subscriber   (SubscriptionsDyn)
import           Gonimo.I18N
import qualified Gonimo.SocketAPI           as API
import           Gonimo.SocketAPI.Types     (FamilyId)
import qualified Gonimo.SocketAPI.Types     as API
import qualified Gonimo.Types               as Gonimo
import           Gonimo.Client.Router (Route(..))


type FamilyMap = Map FamilyId API.Family

data Config t
  = Config { _configResponse      :: Event t API.ServerResponse
           , _configAuthData      :: Dynamic t (Maybe API.AuthData)
           , _configSelectFamily  :: Event t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateFamily  :: Event t ()
           , _configLeaveFamily   :: Event t ()
           , _configSetName       :: Event t Text
           }

data Family t
  = Family { _families       :: Dynamic t (Maybe FamilyMap)
           , _selectedFamily :: Dynamic t (Maybe FamilyId)
           , _subscriptions  :: SubscriptionsDyn t
           , _request        :: Event t [ API.ServerRequest ]
           }

data UI t
  = UI { _uiSelectFamily  :: Event t FamilyId
       , _uiCreateFamily  :: Event t ()
       , _uiLeaveFamily   :: Event t ()
       , _uiSetName       :: Event t Text
       , _uiRouteSelected :: Event t Route
       , _uiRequest       :: Event t [ API.ServerRequest ]
       , _uiSelectLang    :: Event t Locale
       }

data CreateFamilyResult = CreateFamilyCancel | CreateFamilyOk | CreateFamilySetName !Text

data DefiniteFamily t
  = DefiniteFamily { _definiteFamilies :: Dynamic t FamilyMap
                   , _definiteSelected :: Dynamic t FamilyId
                   }


instance Reflex t => Default (UI t) where
  def = UI never never never never never never never

fromApp :: Reflex t => App.Model t -> Config t
fromApp c = Config { _configResponse = c^.onResponse
                   , _configAuthData = c^.Auth.authData
                   , _configSelectFamily = never
                   , _configSetName = never
                   , _configAuthenticated = c^.Auth.onAuthenticated
                   , _configCreateFamily = never
                   , _configLeaveFamily = never
                   }

uiSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (UI t) -> m (UI t)
uiSwitchPromptly ev
  = UI <$> switchPromptly never (_uiSelectFamily <$> ev)
       <*> switchPromptly never (_uiCreateFamily <$> ev)
       <*> switchPromptly never (_uiLeaveFamily <$> ev)
       <*> switchPromptly never (_uiSetName <$> ev)
       <*> switchPromptly never (_uiRouteSelected <$> ev)
       <*> switchPromptly never (_uiRequest <$> ev)
       <*> switchPromptly never (_uiSelectLang <$> ev)

uiSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (UI t) -> UI t
uiSwitchPromptlyDyn ev
  = UI (switchPromptlyDyn (_uiSelectFamily <$> ev))
       (switchPromptlyDyn (_uiCreateFamily <$> ev))
       (switchPromptlyDyn (_uiLeaveFamily <$> ev))
       (switchPromptlyDyn (_uiSetName <$> ev))
       (switchPromptlyDyn (_uiRouteSelected <$> ev))
       (switchPromptlyDyn (_uiRequest <$> ev))
       (switchPromptlyDyn (_uiSelectLang <$> ev))

-- makeDefinite :: forall m t. Reflex t => Family t -> Dynamic t (Maybe DefiniteFamily t)
-- makeDefinite family' =
--   let
--     onlyJusts = push (pure . id) . updated
--     result = zipDynWith make' (family'^.families) (family'^.selectedFamily)
--     make' mFam mSel = do
--       famInit <- mFam
--       selInit <- mSel
--       pure $ DefiniteFamily { definiteFamilies = hold}

currentFamilyName :: forall t. Reflex t => DefiniteFamily t -> Dynamic t Text
currentFamilyName df =
    let
      getFamilyName :: FamilyId -> Map FamilyId API.Family -> Text
      getFamilyName fid families' = families'^.at fid._Just.to API.familyName . to Gonimo.familyNameName
    in
      zipDynWith getFamilyName (df^.definiteSelected) (df^.definiteFamilies)

family :: forall model m t. (GonimoM model t m) => Config t -> m (Family t)
family config' = do
    (subs, familyIds', families') <- makeFamilies config'
    -- We need to use ids here, because we need intermediate information not available in families'
    (selectReqs, selected') <- handleFamilySelect config' familyIds'
    let createReqs = handleCreateFamily config'
    let leaveReqs = handleLeaveFamily config' selected'
    let renameReqs = handleSetName config' selected'
    pure $ Family { _subscriptions = subs
                  , _families = families'
                  , _selectedFamily = selected'
                  , _request = selectReqs <> createReqs <> leaveReqs <> renameReqs
                }

handleFamilySelect :: forall model m t. GonimoM model t m
                      => Config t -> Dynamic t (Maybe [FamilyId])
                      -> m (Event t [ API.ServerRequest ], Dynamic t (Maybe FamilyId))
handleFamilySelect config' familyIds' = mdo
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
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

    let selectEvent = leftmost [ Just <$> config'^.configSelectFamily, switchOnChange ]
    selected <- holdDyn loadedFamilyId selectEvent
    reqs <- makeRequest selected

    pure (reqs, selected)
  where
    makeRequest :: Dynamic t (Maybe FamilyId) -> m (Event t [ API.ServerRequest ])
    makeRequest selected' = do
      let
        mayReq :: Maybe API.AuthData -> Maybe FamilyId -> Maybe API.ServerRequest
        mayReq mAuth mFid = API.ReqSwitchFamily <$> fmap API.deviceId mAuth <*> mFid

        reqDyn = zipDynWith mayReq (config'^.configAuthData) selected'
        reqsDyn :: Dynamic t [ API.ServerRequest ]
        reqsDyn = maybe [] (:[]) <$> reqDyn

      pure $ mconcat $ [ tag (current reqsDyn) $ config'^.configAuthenticated
                       , updated reqsDyn
                       ]

handleCreateFamily :: forall t. Reflex t => Config t -> Event t [ API.ServerRequest ]
handleCreateFamily config' = const [API.ReqCreateFamily] <$> config'^.configCreateFamily

handleLeaveFamily :: forall t. Reflex t => Config t -> Dynamic t (Maybe FamilyId) -> Event t [ API.ServerRequest ]
handleLeaveFamily config' selected'
  = push (\_ -> runMaybeT $ do
             auth <- MaybeT . sample $ current (config'^.configAuthData)
             selected <- MaybeT . sample $ current selected'
             let req = API.ReqLeaveFamily (API.accountId auth) selected
             pure [req]
         ) (config'^.configLeaveFamily)

handleSetName :: forall t. Reflex t => Config t -> Dynamic t (Maybe FamilyId) -> Event t [ API.ServerRequest ]
handleSetName config' selectedFamily' = push (\name -> do
                                                mFid <- sample $ current selectedFamily'
                                                case mFid of
                                                  Nothing -> pure Nothing
                                                  Just fid -> pure $ Just [ API.ReqSetFamilyName fid name ]
                                            ) (config'^.configSetName)

makeFamilies :: forall model m t. GonimoM model t m
                => Config t -> m (SubscriptionsDyn t, Dynamic t (Maybe [FamilyId]), Dynamic t (Maybe FamilyMap))
makeFamilies config' = do
  (subs, fids) <- makeSubscriptions
  families' <- makeFamilyMap fids
  pure (subs, fids, families')
 where
   makeSubscriptions :: m (SubscriptionsDyn t, Dynamic t (Maybe [FamilyId]))
   makeSubscriptions = do
     let
       handleGotFamilies resp = case resp of
         API.ResGotFamilies _ fids -> pure . Just $ fids
         _                         -> pure Nothing
       gotFamiliesEvent = push handleGotFamilies (config'^.configResponse)

       fidsToSubscriptions = foldr Set.insert Set.empty . map API.ReqGetFamily

     familyIds <- holdDyn [] gotFamiliesEvent
     let familiesSubs = fidsToSubscriptions <$> familyIds

     let familyIdsSubs = maybe Set.empty (Set.singleton . API.ReqGetFamilies . API.accountId)
                          <$> config'^.configAuthData

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

       familyMapEv = push resToFamily $ config'^.configResponse
     familyMap <- holdDyn Map.empty familyMapEv
     pure $ zipDynWith makeConsistent familyMap allFids



-- Prisms:


_CreateFamilyCancel :: Prism' CreateFamilyResult ()
_CreateFamilyCancel = prism' (\() -> CreateFamilyCancel) go
  where
    go c = case c of
      CreateFamilyCancel -> Just ()
      _                  -> Nothing

_CreateFamilyOk :: Prism' CreateFamilyResult ()
_CreateFamilyOk = prism' (\() -> CreateFamilyOk) go
  where
    go c = case c of
      CreateFamilyOk -> Just ()
      _              -> Nothing

_CreateFamilySetName :: Prism' CreateFamilyResult Text
_CreateFamilySetName = prism' (\t -> CreateFamilySetName t) go
  where
    go c = case c of
      CreateFamilySetName t -> Just $ t
      _                     -> Nothing

-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthData :: Lens' (Config t) (Dynamic t (Maybe API.AuthData))
configAuthData f config' = (\configAuthData' -> config' { _configAuthData = configAuthData' }) <$> f (_configAuthData config')

configSelectFamily :: Lens' (Config t) (Event t FamilyId)
configSelectFamily f config' = (\configSelectFamily' -> config' { _configSelectFamily = configSelectFamily' }) <$> f (_configSelectFamily config')

configAuthenticated :: Lens' (Config t) (Event t ())
configAuthenticated f config' = (\configAuthenticated' -> config' { _configAuthenticated = configAuthenticated' }) <$> f (_configAuthenticated config')

configCreateFamily :: Lens' (Config t) (Event t ())
configCreateFamily f config' = (\configCreateFamily' -> config' { _configCreateFamily = configCreateFamily' }) <$> f (_configCreateFamily config')

configLeaveFamily :: Lens' (Config t) (Event t ())
configLeaveFamily f config' = (\configLeaveFamily' -> config' { _configLeaveFamily = configLeaveFamily' }) <$> f (_configLeaveFamily config')

configSetName :: Lens' (Config t) (Event t Text)
configSetName f config' = (\configSetName' -> config' { _configSetName = configSetName' }) <$> f (_configSetName config')


-- Lenses for Family t:

families :: Lens' (Family t) (Dynamic t (Maybe FamilyMap))
families f family' = (\families' -> family' { _families = families' }) <$> f (_families family')

selectedFamily :: Lens' (Family t) (Dynamic t (Maybe FamilyId))
selectedFamily f family' = (\selectedFamily' -> family' { _selectedFamily = selectedFamily' }) <$> f (_selectedFamily family')

subscriptions :: Lens' (Family t) (SubscriptionsDyn t)
subscriptions f family' = (\subscriptions' -> family' { _subscriptions = subscriptions' }) <$> f (_subscriptions family')

request :: Lens' (Family t) (Event t [ API.ServerRequest ])
request f family' = (\request' -> family' { _request = request' }) <$> f (_request family')


-- Lenses for UI t:

uiSelectFamily :: Lens' (UI t) (Event t FamilyId)
uiSelectFamily f uI' = (\uiSelectFamily' -> uI' { _uiSelectFamily = uiSelectFamily' }) <$> f (_uiSelectFamily uI')

uiCreateFamily :: Lens' (UI t) (Event t ())
uiCreateFamily f uI' = (\uiCreateFamily' -> uI' { _uiCreateFamily = uiCreateFamily' }) <$> f (_uiCreateFamily uI')

uiLeaveFamily :: Lens' (UI t) (Event t ())
uiLeaveFamily f uI' = (\uiLeaveFamily' -> uI' { _uiLeaveFamily = uiLeaveFamily' }) <$> f (_uiLeaveFamily uI')

uiSetName :: Lens' (UI t) (Event t Text)
uiSetName f uI' = (\uiSetName' -> uI' { _uiSetName = uiSetName' }) <$> f (_uiSetName uI')

uiRouteSelected :: Lens' (UI t) (Event t Route)
uiRouteSelected f uI' = (\uiRouteSelected' -> uI' { _uiRouteSelected = uiRouteSelected' }) <$> f (_uiRouteSelected uI')

uiRequest :: Lens' (UI t) (Event t [ API.ServerRequest ])
uiRequest f uI' = (\uiRequest' -> uI' { _uiRequest = uiRequest' }) <$> f (_uiRequest uI')

uiSelectLang :: Lens' (UI t) (Event t Locale)
uiSelectLang f uI' = (\uiSelectLang' -> uI' { _uiSelectLang = uiSelectLang' }) <$> f (_uiSelectLang uI')


-- Lenses for DefiniteFamily t:

definiteFamilies :: Lens' (DefiniteFamily t) (Dynamic t FamilyMap)
definiteFamilies f definiteFamily' = (\definiteFamilies' -> definiteFamily' { _definiteFamilies = definiteFamilies' }) <$> f (_definiteFamilies definiteFamily')

definiteSelected :: Lens' (DefiniteFamily t) (Dynamic t FamilyId)
definiteSelected f definiteFamily' = (\definiteSelected' -> definiteFamily' { _definiteSelected = definiteSelected' }) <$> f (_definiteSelected definiteFamily')


