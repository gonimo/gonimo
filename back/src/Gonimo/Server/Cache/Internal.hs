{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Gonimo.Server.Cache.Internal
Description : Types and internal function for "Gonimo.Server.Cache"
Copyright   : (c) Robert Klotzner, 2017

-}
module Gonimo.Server.Cache.Internal where

import           Control.Lens
import           Data.Map                           (Map)
import qualified Data.Map                           as Map


import           Reflex hiding (Response, Request)



import           Gonimo.Prelude
import           Gonimo.Server.Cache.Devices        (Devices)
import qualified Gonimo.Server.Cache.Devices        as Devices
import           Gonimo.Server.Cache.FamilyAccounts (FamilyAccounts)
import qualified Gonimo.Server.Cache.FamilyAccounts as FamilyAccounts
import qualified Gonimo.Server.Cache.IndexedTable   as Table
import           Gonimo.Server.Cache.Invitations    (Invitations)
import qualified Gonimo.Server.Cache.Invitations    as Invitations
import           Gonimo.SocketAPI.Model
import           Gonimo.Server.Db.Internal (ModelDump (..), HasModelDump (..))



data Config t
  = Config { -- | Load new data into the model.
             --
             --   Please beware that any entries already present in the cache
             --   will be overriden with entries with the same id from the dump.
             --   This should be fine if you take care of only updating the
             --   cache after data has been written to the database.
             _onLoadData  :: Event t ModelDump

             -- | Update some data in the model.
           , _onUpdate    :: Event t (Model -> Model)

             -- | Replace the current model with new data.
             --
             --   This is used in "Gonimo.Server.Cache.GC" when collecting
             --   garbage. The GC simply replaces the model with live data and the
             --   old data gets discarded.
           , _onLoadModel :: Event t Model
           }


-- | The cache as a result of the events in 'Config'.
type Cache t = Behavior t Model

-- | Cached database data
data Model
  = Model { _families       :: Families
          , _invitations    :: Invitations
          , _accounts       :: Accounts
          , _familyAccounts :: FamilyAccounts
          , _devices        :: Devices
          }

-- | Families as representend in the cached model.
type Families = Map FamilyId Family

-- | Accounts as represented in the cached model.
type Accounts = Map AccountId Account



-- | Model not holding any data.
emptyModel :: Model
emptyModel = Model { _families = Map.empty
                   , _invitations = Invitations.make Map.empty
                   , _accounts = Map.empty
                   , _familyAccounts = FamilyAccounts.make Map.empty
                   , _devices = Devices.make Map.empty
                   }

-- | Load a data dump into the model.
--
--   Load a dump into the model, if some id in the dump is already present in
--   the model, the data in the model will be overridden.
loadDump :: ModelDump -> Model -> Model
loadDump dump = (families       %~ (Map.union . Map.fromList)  (dump^.dumpedFamilies))
              . (invitations    %~ Table.loadData (dump^.dumpedInvitations))
              -- . (accounts       %~ loadOnlyNewMap (dump^.dumpedAccounts))
              . familyAccounts  %~ Table.loadData (dump^.dumpedFamilyAccounts)
              . devices         %~ Table.loadData (dump^.dumpedDevices)

-- -- Old implementation filtering out already present data:
-- loadDump dump = (families       %~ loadOnlyNewMap (dump^.dumpedFamilies))
--               . (invitations    %~ loadOnlyNew (dump^.dumpedInvitations))
--               -- . (accounts       %~ loadOnlyNewMap (dump^.dumpedAccounts))
--               . (familyAccounts %~ loadOnlyNew (dump^.dumpedFamilyAccounts))
--               . (devices        %~ loadOnlyNew (dump^.dumpedDevices))
--   where
--     loadOnlyNew dumped table' = Table.loadData (filterDump table' dumped) table'
--     loadOnlyNewMap = Map.unionWith (const id) . Map.fromList

--     filterDump :: At s => s -> [(Index s, b)] -> [(Index s, b)]
--     filterDump table' = filter (isNothing . (table'^.) . at . fst)

{-
loadFamilyData :: (Server.HasConfig c , MonadAppHost t m)
               => c
               -> Event t FamilyId
               -> m (Event t (FamilyId, Maybe ModelDump))
loadFamilyData c onFamilyId = App.performEventAsync $ withFamilyId doLoad <$> onFamilyId
  where
    withFamilyId :: (FamilyId -> IO (Maybe FamilyLoad)) -> FamilyId -> IO (FamilyId, Maybe ModelDump)
    withFamilyId load = runKleisli (Kleisli pure &&& Kleisli load)

    doLoad :: FamilyId -> IO (Maybe FamilyLoad)
    doLoad = Server.mayRunDb c . loadFamilyData'

loadFamilyData' :: (MonadIO m, MonadBase IO m) => Model -> FamilyId -> ReaderT SqlBackend m ModelDump
loadFamilyData' model fid = do
  family' <- Family.get fid
  familyAccounts' :: [(FamilyAccountId, FamilyAccount)] <- getFamilyAccountsFiltered fid
  let
    accountIds :: [AccountId]
    accountIds = getAccountIdsFiltered familyAccounts'

  accounts' :: [Account] <- traverse Account.get accountIds
  devices' :: [(DeviceId, Device)] <- getAllDevicesFiltered accountIds
  invitations' :: [(InvitationId, Invitation)] <- getInvitationsFiltered fid

  pure $ ModelDump { _dumpedFamilies = [(fid, family')]
                   , _dumpedAccounts = zip accountIds accounts'
                   , _dumpedFamilyAccounts = familyAccounts'
                   , _dumpedInvitations = invitations'
                   , _dumpedFamilyDevices = devices'
                   }
  where
    getFamilyAccountsFiltered = fmap (filterAlreadyLoaded fst (model^.familyAccounts)) . Family.getFamilyAccounts

    getAccountIdsFiltered = filterAlreadyLoaded id (model^.accounts)
                            . map (familyAccountAccountId . snd)

    getAllDevicesFiltered = fmap (filterAlreadyLoaded fst (model^.devices) . concat) . traverse Account.getDevices

    getInvitationsFiltered = fmap (filterAlreadyLoaded fst (model^.invitations)) . Family.getInvitations

filterAlreadyLoaded :: At (c key val) => (a -> key) -> c -> [a] -> [a]
filterAlreadyLoaded getKey loadedData = filter (isNothing . (loadedData^.) . at . getKey)

-}

-- * Lenses:
-- * Automatically generated lenses:



class HasConfig a where
  config :: Lens' (a t) (Config t)

  onLoadData :: Lens' (a t) (Event t ModelDump)
  onLoadData = config . go
    where
      go :: Lens' (Config t) (Event t ModelDump)
      go f config' = (\onLoadData' -> config' { _onLoadData = onLoadData' }) <$> f (_onLoadData config')


  onUpdate :: Lens' (a t) (Event t (Model -> Model))
  onUpdate = config . go
    where
      go :: Lens' (Config t) (Event t (Model -> Model))
      go f config' = (\onUpdate' -> config' { _onUpdate = onUpdate' }) <$> f (_onUpdate config')


  onLoadModel :: Lens' (a t) (Event t Model)
  onLoadModel = config . go
    where
      go :: Lens' (Config t) (Event t Model)
      go f config' = (\onLoadModel' -> config' { _onLoadModel = onLoadModel' }) <$> f (_onLoadModel config')


instance HasConfig Config where
  config = id

class HasModel a where
  model :: Lens' a Model

  families :: Lens' a Families
  families = model . go
    where
      go :: Lens' Model Families
      go f model' = (\families' -> model' { _families = families' }) <$> f (_families model')


  invitations :: Lens' a Invitations
  invitations = model . go
    where
      go :: Lens' Model Invitations
      go f model' = (\invitations' -> model' { _invitations = invitations' }) <$> f (_invitations model')


  accounts :: Lens' a Accounts
  accounts = model . go
    where
      go :: Lens' Model Accounts
      go f model' = (\accounts' -> model' { _accounts = accounts' }) <$> f (_accounts model')


  familyAccounts :: Lens' a FamilyAccounts
  familyAccounts = model . go
    where
      go :: Lens' Model FamilyAccounts
      go f model' = (\familyAccounts' -> model' { _familyAccounts = familyAccounts' }) <$> f (_familyAccounts model')


  devices :: Lens' a Devices
  devices = model . go
    where
      go :: Lens' Model Devices
      go f model' = (\devices' -> model' { _devices = devices' }) <$> f (_devices model')


instance HasModel Model where
  model = id

