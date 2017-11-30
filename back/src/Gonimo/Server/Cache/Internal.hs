{-|
Module      : Gonimo.Server.Cache.Internal
Description : Types and internal function for "Gonimo.Server.Cache"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.Internal where

import Reflex
import qualified Reflex.Host.App as App

import Gonimo.SocketAPI
import Gonimo.SocketAPI.Model

data Config t
  = Config { -- | Load all data from the Db related to a single family. (Family
             --   data, accounts, invitations, devices, ...)
             _onLoadFamilyData :: Event t FamilyId

             -- | Load all data related to a single account: Account data, devices, families.
           , _onLoadAccountData :: Event t AccountId

             -- | Cache will be updated according to incoming updates.
           , _onUpdate :: Event t Update

           , _serverConfig :: Server.Config t
           }

data Cache t
  = Cache { _families            :: Behavior t Families
          , _invitations         :: Behavior t Invitations
          , _familyInvitations   :: Behavior t FamilyInvitations
          , _accountInvitations  :: Behavior t AccountInvitations
          , _accounts            :: Behavior t Accounts
          , _familyAccounts      :: Behavior t FamilyAccounts
          , _accountFamilies     :: Behavior t AccountFamilies
          , _familyAccountData   :: Behavior t FamilyAccountData

          , _onLoadedFamilyData  :: Event t FamilyId
          , _onLoadedAccountData :: Event t AccountId
          }

type Families = Map FamilyId Family
type Invitations = Map InvitationId Invitation
type FamilyInvitations = Map FamilyId [InvitationId]

-- | Claimed invitations of an account.
type AccountInvitations = Map AccountId [InvitationId]

type Accounts = Map AccountId Account
type FamilyAccounts = Map FamilyId [AccountId]
type AccountFamilies = Map AccountId [FamilyId]
type FamilyAccountData = Map (FamilyId, AccountId) FamilyAccount

make :: (MonadServer m, Reflex t) => Config t -> m (Cache t)
make conf = undefined


data FamilyLoad
  = FamilyLoad { _loadedFamily            :: !(FamilyId, Family)
               , _loadedAccounts          :: !Accounts
               , _loadedFamilyAccountData :: !FamilyAccountData
               , _loadedFamilyInvitations :: !Invitations
               , _loadedFamilyDevices     :: !Devices
               }

loadFamilyData :: (Server.HasConfig c , Reflex t)
               => c
               -> Event t FamilyId
               -> m (Event t (FamilyId, Maybe FamilyLoad))
loadFamilyData serverConf onFamilyId = performEventAsync $ loadFamilyData' <$> onFamilyId
  where
    doLoad = fmap Just . runDb serverConf . loadFamilyData'
    safeLoad fid = doLoad fid `catch` logError

    logError :: SomeException -> IO (Maybe FamilyLoad)
    logError e = 

    forkIt sendResult fid = void . forkIO $ do
      liftIO . sendResult =<< loadFamilyData' fid

loadFamilyData' :: (MonadIO m) => FamilyId -> ReaderT SqlBackend m FamilyLoad
loadFamilyData' fid = do
  family' <- Family.get fid
  familyAccounts' :: [FamilyAccount] <- Family.getFamilyAccounts fid
  let
    familyAccounts :: [((FamilyId, AccountId), FamilyAccount)]
    familyAccounts = ((fid,) . familyAccountAccountId &&& id ) familyAccounts'

    accountIds :: [AccountId]
    accountIds = familyAccountAccountId <$> familyAccounts

  accounts :: [Account] <- traverse Account.get accountIds
  devices :: [(DeviceId, Device)] <- concat <$> traverse Account.getDevices accountIds
  invitations :: [(InvitationId, Invitation)] <- Family.getInvitations fid

  pure $ FamilyLoad { _loadedFamily = family'
                    , _loadedAccounts = Map.fromList $ zip accountIds accounts
                    , _loadedFamilyAccountData = Map.fromList familyAccounts
                    , _loadedFamilyInvitations = Map.fromList invitations
                    , _loadedFamilyDevices = Map.fromList devices
                    }

-- * Lenses:

instance Server.HasConfig Config where
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


  serverConfig :: Lens' (a t) (Server.Config t)
  serverConfig = config . go
    where
      go :: Lens' (Config t) (Server.Config t)
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


  familyInvitations :: Lens' (a t) (Behavior t FamilyInvitations)
  familyInvitations = cache . go
    where
      go :: Lens' (Cache t) (Behavior t FamilyInvitations)
      go f cache' = (\familyInvitations' -> cache' { _familyInvitations = familyInvitations' }) <$> f (_familyInvitations cache')


  accountInvitations :: Lens' (a t) (Behavior t AccountInvitations)
  accountInvitations = cache . go
    where
      go :: Lens' (Cache t) (Behavior t AccountInvitations)
      go f cache' = (\accountInvitations' -> cache' { _accountInvitations = accountInvitations' }) <$> f (_accountInvitations cache')


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


  accountFamilies :: Lens' (a t) (Behavior t AccountFamilies)
  accountFamilies = cache . go
    where
      go :: Lens' (Cache t) (Behavior t AccountFamilies)
      go f cache' = (\accountFamilies' -> cache' { _accountFamilies = accountFamilies' }) <$> f (_accountFamilies cache')


  familyAccountData :: Lens' (a t) (Behavior t FamilyAccountData)
  familyAccountData = cache . go
    where
      go :: Lens' (Cache t) (Behavior t FamilyAccountData)
      go f cache' = (\familyAccountData' -> cache' { _familyAccountData = familyAccountData' }) <$> f (_familyAccountData cache')


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

