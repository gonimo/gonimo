{-# LANGUAGE TupleSections #-}
{-|
Module      : Gonimo.Server.Cache.Internal
Description : Types and internal function for "Gonimo.Server.Cache"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.Internal where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Base         (MonadBase)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Database.Persist.Sql       (SqlBackend)
import           Reflex
import           Reflex.Host.App            (MonadAppHost)
import qualified Reflex.Host.App            as App



import Gonimo.SocketAPI
import Gonimo.SocketAPI.Model
import qualified Gonimo.Server.Db.Family as Family
import qualified Gonimo.Server.Db.Account as Account
import qualified Gonimo.Server.Config as Server
import           Gonimo.Server.Cache.Invitations (Invitations)
import qualified Gonimo.Server.Cache.Invitations as Invitations
import           Gonimo.Server.Cache.FamilyAccounts (FamilyAccounts)
import qualified Gonimo.Server.Cache.FamilyAccounts as FamilyAccounts
import           Gonimo.Server.Cache.Devices (Devices)
import qualified Gonimo.Server.Cache.Devices as Devices



data Config t
  = Config { -- | Load all data from the Db related to a single family. (Family
             --   data, accounts, invitations, devices, ...)
             _onLoadFamilyData :: Event t FamilyId

             -- | Load all data related to a single account: Account data, devices, families.
           , _onLoadAccountData :: Event t AccountId

             -- | Cache will be updated according to incoming updates.
           , _onUpdate :: Event t Update

           , _serverConfig :: Server.Config
           }


data Cache t
  = Cache { _families            :: Behavior t Families
          , _invitations         :: Behavior t Invitations
          , _accounts            :: Behavior t Accounts
          , _familyAccounts      :: Behavior t FamilyAccounts
          , _devices             :: Behavior t Devices

          , _onLoadedFamilyData  :: Event t FamilyId
          , _onLoadedAccountData :: Event t AccountId
          }

-- | Sampled data of a cache for covenience.
data Sampled
  = Sampled { _sampledFamilies            :: !Families
            , _sampledInvitations         :: !Invitations
            , _sampledAccounts            :: !Accounts
            , _sampledFamilyAccounts      :: !FamilyAccounts
            , _sampledDevices             :: !Devices
            }

type Families = Map FamilyId Family
type Accounts = Map AccountId Account


-- make :: (MonadAppHost m, Reflex t) => Config t -> m (Cache t)
-- make conf = undefined


data FamilyLoad
  = FamilyLoad { _loadedFamily            :: !Family
               , _loadedAccounts          :: !Accounts
               , _loadedFamilyAccounts    :: !FamilyAccounts
               , _loadedFamilyInvitations :: !Invitations
               , _loadedFamilyDevices     :: !Devices
               }

loadFamilyData :: (Server.HasConfig c , MonadAppHost t m)
               => c
               -> Event t FamilyId
               -> m (Event t (FamilyId, Maybe FamilyLoad))
loadFamilyData c onFamilyId = App.performEventAsync $ withFamilyId doLoad <$> onFamilyId
  where
    withFamilyId :: (FamilyId -> IO (Maybe FamilyLoad)) -> FamilyId -> IO (FamilyId, Maybe FamilyLoad)
    withFamilyId load = runKleisli (Kleisli pure &&& Kleisli load)

    doLoad :: FamilyId -> IO (Maybe FamilyLoad)
    doLoad = Server.mayRunDb c . loadFamilyData'

loadFamilyData' :: (MonadIO m, MonadBase IO m) => FamilyId -> ReaderT SqlBackend m FamilyLoad
loadFamilyData' fid = do
  family' <- Family.get fid
  familyAccounts' :: [(FamilyAccountId, FamilyAccount)] <- Family.getFamilyAccounts fid
  let
    accountIds :: [AccountId]
    accountIds = familyAccountAccountId . snd <$> familyAccounts'

  accounts' :: [Account] <- traverse Account.get accountIds
  devices' :: [(DeviceId, Device)] <- concat <$> traverse Account.getDevices accountIds
  invitations' :: [(InvitationId, Invitation)] <- Family.getInvitations fid

  pure $ FamilyLoad { _loadedFamily = family'
                    , _loadedAccounts = Map.fromList $ zip accountIds accounts'
                    , _loadedFamilyAccounts = FamilyAccounts.make $ Map.fromList familyAccounts'
                    , _loadedFamilyInvitations = Invitations.make $ Map.fromList invitations'
                    , _loadedFamilyDevices = Devices.make $ Map.fromList devices'
                    }

-- * Lenses:

instance Server.HasConfig (Config t) where
  config = serverConfig

class HasConfig a where
  config :: Lens' (a t) (Config t)

  onLoadFamilyData :: Lens' (a t) (Event t FamilyId)
  onLoadFamilyData = config . go
    where
      go :: Lens' (Config t) (Event t FamilyId)
      go f config' = (\onLoadFamilyData' -> config' { _onLoadFamilyData = onLoadFamilyData' }) <$> f (_onLoadFamilyData config')


  onLoadAccountData :: Lens' (a t) (Event t AccountId)
  onLoadAccountData = config . go
    where
      go :: Lens' (Config t) (Event t AccountId)
      go f config' = (\onLoadAccountData' -> config' { _onLoadAccountData = onLoadAccountData' }) <$> f (_onLoadAccountData config')


  onUpdate :: Lens' (a t) (Event t Update)
  onUpdate = config . go
    where
      go :: Lens' (Config t) (Event t Update)
      go f config' = (\onUpdate' -> config' { _onUpdate = onUpdate' }) <$> f (_onUpdate config')


  serverConfig :: Lens' (a t) Server.Config
  serverConfig = config . go
    where
      go :: Lens' (Config t) Server.Config
      go f config' = (\serverConfig' -> config' { _serverConfig = serverConfig' }) <$> f (_serverConfig config')


instance HasConfig Config where
  config = id

class HasCache a where
  cache :: Lens' (a t) (Cache t)

  families :: Lens' (a t) (Behavior t Families)
  families = cache . go
    where
      go :: Lens' (Cache t) (Behavior t Families)
      go f cache' = (\families' -> cache' { _families = families' }) <$> f (_families cache')


  invitations :: Lens' (a t) (Behavior t Invitations)
  invitations = cache . go
    where
      go :: Lens' (Cache t) (Behavior t Invitations)
      go f cache' = (\invitations' -> cache' { _invitations = invitations' }) <$> f (_invitations cache')


  accounts :: Lens' (a t) (Behavior t Accounts)
  accounts = cache . go
    where
      go :: Lens' (Cache t) (Behavior t Accounts)
      go f cache' = (\accounts' -> cache' { _accounts = accounts' }) <$> f (_accounts cache')


  familyAccounts :: Lens' (a t) (Behavior t FamilyAccounts)
  familyAccounts = cache . go
    where
      go :: Lens' (Cache t) (Behavior t FamilyAccounts)
      go f cache' = (\familyAccounts' -> cache' { _familyAccounts = familyAccounts' }) <$> f (_familyAccounts cache')


  devices :: Lens' (a t) (Behavior t Devices)
  devices = cache . go
    where
      go :: Lens' (Cache t) (Behavior t Devices)
      go f cache' = (\devices' -> cache' { _devices = devices' }) <$> f (_devices cache')


  onLoadedFamilyData :: Lens' (a t) (Event t FamilyId)
  onLoadedFamilyData = cache . go
    where
      go :: Lens' (Cache t) (Event t FamilyId)
      go f cache' = (\onLoadedFamilyData' -> cache' { _onLoadedFamilyData = onLoadedFamilyData' }) <$> f (_onLoadedFamilyData cache')


  onLoadedAccountData :: Lens' (a t) (Event t AccountId)
  onLoadedAccountData = cache . go
    where
      go :: Lens' (Cache t) (Event t AccountId)
      go f cache' = (\onLoadedAccountData' -> cache' { _onLoadedAccountData = onLoadedAccountData' }) <$> f (_onLoadedAccountData cache')


instance HasCache Cache where
  cache = id

class HasSampled a where
  sampled :: Lens' a Sampled

  sampledFamilies :: Lens' a Families
  sampledFamilies = sampled . go
    where
      go :: Lens' Sampled Families
      go f sampled' = (\sampledFamilies' -> sampled' { _sampledFamilies = sampledFamilies' }) <$> f (_sampledFamilies sampled')


  sampledInvitations :: Lens' a Invitations
  sampledInvitations = sampled . go
    where
      go :: Lens' Sampled Invitations
      go f sampled' = (\sampledInvitations' -> sampled' { _sampledInvitations = sampledInvitations' }) <$> f (_sampledInvitations sampled')


  sampledAccounts :: Lens' a Accounts
  sampledAccounts = sampled . go
    where
      go :: Lens' Sampled Accounts
      go f sampled' = (\sampledAccounts' -> sampled' { _sampledAccounts = sampledAccounts' }) <$> f (_sampledAccounts sampled')


  sampledFamilyAccounts :: Lens' a FamilyAccounts
  sampledFamilyAccounts = sampled . go
    where
      go :: Lens' Sampled FamilyAccounts
      go f sampled' = (\sampledFamilyAccounts' -> sampled' { _sampledFamilyAccounts = sampledFamilyAccounts' }) <$> f (_sampledFamilyAccounts sampled')


  sampledDevices :: Lens' a Devices
  sampledDevices = sampled . go
    where
      go :: Lens' Sampled Devices
      go f sampled' = (\sampledDevices' -> sampled' { _sampledDevices = sampledDevices' }) <$> f (_sampledDevices sampled')


instance HasSampled Sampled where
  sampled = id

