{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Gonimo.Server where

data Clients t
  = Clients { _onReceived :: Event t (DeviceId, FromClient)
              -- | A device came online:
            , _onCreatedClient :: Event t DeviceId
              -- | A device went offline:
            , _onRemovedClient :: Event t DeviceId
            , _onlineStatus :: Behavior t (Map DeviceId DeviceStatus)
            , _selectedFamily :: Behavior t (Map DeviceId FamilyId)
            , _bySelectedFamily :: Behavior t (Map FamilyId DeviceId) -- ^ Needed for routing messages to concerned devices.
            -- For notifying devices that they have been kicked out from a family they don't have currently selected,
            -- We just look up the kicked device in clients and notify it.
            -- Therefore with _bySelectedFamily the event route has all needed information for routing all events to concerend parties.
            }

-- Authorizer:

data Config t
  = Config { _onAuthorize :: Event t (DeviceId, FromClient)
           }

data Authorizer t
  = Authorizer { _onForbidden :: Event t (DeviceId, ToClient)
               , _onAuthorized :: Event t FromClient
               }

-- Server:
make = do
  clients <- Clients.make clientConfig
  authorized <- Authorizer.make authConfig
  authorized >>= requesthandler

data LoadOrRead
  = LoadFamily !FamilyId
  | LoadAccount !AccountId
  | Read !View

type FamilyWaiters = Map FamilyId [DeviceId]

newtype EventListeners event listener = EventListeners (Map event [listener])

waitOn :: (Eq event, Ord event) => event -> listener -> EventListeners event listener -> EventListeners event listener
waitOn ev l (EventListeners m) = m & at ev . non [] %~ l

trigger :: (Eq event, Ord event) => event -> EventListeners event listener -> 


-- Cache:

{--
  Client requests some data ('Get'):
  Either family or account data needs to be fetched if not yet there.

-> loadInCache:
 -- Data there: Ready event - done
 -- Data not there:
    -- Issue load
    -- Queue request
    -- onload: send ready event
    -- Remove requests from queue.
--}

type Queues k v = Map k [v]

newtype Cache t k v = Cache { runCache :: Behavior t (Map k v)
                            }

newtype LoaderConfig t k request
  = LoaderConfig {
      -- | E.g. for a Behavior map you can define '_isCached' like so:
      --
      -- @
      --   LoaderConfig { _isCached = flip Map.member <$> someMapBehavior
                        }
      -- @
      _isCached :: Behavior t (k -> Bool)

      -- | Event from the cache manager, if this event is triggered it is
      --   assumed that the requested data can be found in the cache already.
    , _onLoaded :: Event t k

      -- | User event that should be delayed until the data specified by k is available in the cache.
    , _onCache :: Event t (k, request)
    }

newtype Loader t k request
  = Loader { _onLoadRequest :: Event t k -- ^ Load request to be handled by the cache manager.
           , _onCached :: Event t [request] -- ^ Delayed input events. When triggered the needed data is available in the cache (if the given key was valid in the first place.)
           }

-- | Delays an event until the required data is cached.
--
--   When the delayed event triggers, either the data is in cache or it was not
--   found, result: If the data is not in the cache then, it wasn't in the
--   database either.
load :: forall m t k request. (Reflex t, MonadHold t m)
     => LoaderConfig t k request -> Loader t k request
load conf = do
    let
      inCache :: Event t ((k, request), Bool)
      inCache = attachWith checkCached (conf^.isCached) (conf^.onCache)

      needsLoad, alreadyLoaded :: Event t (k, request)
      needsLoad      = not `cached` inCache
      alreadyLoaded  = is  `cached` inCache

      _onLoadRequest = fst <$> needsLoad

    queue <- foldBehavior id Map.empty
      $ mergeWith (.) [ queueReq <$> needsLoad
                      , unqueueReqs <$> conf^.onLoaded
                      ]
    let
      readyRequests = attachWith (\q k -> q^.at k. non []) queue (conf^.onLoaded)
      bornReadyRequests = (:[]) . snd <$> alreadyLoaded

      _onCached = mconcat [ readyRequests
                          , bornReadyRequests
                          ]
    pure $ Loader { .. }
  where
    checkCached :: (k -> Bool) -> (k, request) -> ((k, request), Bool)
    checkCached check entry@(key, _) = (entry, check key)

    cached :: forall a. (Bool -> Bool) -> Event t (a, Bool) -> Event t a
    cached f = fmap fst . ffilter (f . snd)

    is = id

    queueReq :: (k, request) -> Queues k request -> Queues k request
    queueReq (k, req) = at k . non [] %~ (req:)

    unqueueReqs :: k -> Queues k request -> Queues k request
    unqueueReqs k = at k .~ Nothing

handleFamilyLoad :: Event t (DeviceId, ViewSelector) -> m ( Event t (DeviceId, View),
                                                          , Event t FamilyId
                                                          , Event t AccountId
                                                          )
handleFamilyLoad cache onLoad = do
    let onLoadOrRead = pushAlways loadOrRead onLoad
    let onFamilyLoaded = 
    familyWaitingLoad <- foldBehavior id Map.empty . mergeWith (.) $
      [ push (pure . fmap (\p -> traverse (Map.insert (p^._1)) (p^?_2._LoadFamily))) onLoadOrRead
      , Map.delete <$> cache^.onLoadedFamilyData
      ]
  where
    addToWaiting :: key -> val -> Map key (Set val) -> Map key (Set val)
    addToWaiting k v = at k.non Set.empty %~ Set.insert v

    removeFromWaiting :: key -> val -> Map key (Set val) -> Map key (Set val)
    removeFromWaiting k v = at k . non Set.empty %~ Set.delete v

    loadOrRead (deviceId, selector) = do
      accountOrFamily <- eitherFamilyAccount selector
      case accountOrFamily of
        Left famId -> do
          families <- sample $ cache^.families
          if Map.member famId families
            then Read . (deviceId, ) <$> getCachedSelectorData selector
            else pure $ LoadFamily famId
        Right accId -> do
          accounts <- sample $ cache^.accounts
          if Map.member accId accounts
            then Read . (deviceId, ) <$> getCachedSelectorData selector
            else pure $ LoadAccount accId



-- Cache:

data Config t
  = Config { -- | Load all data from the Db related to a single family. (Family data, accounts, invitations, devices, ...)
             _onLoadFamilyData :: Event t FamilyId
             -- | Load all data related to a single account: Account data, devices, families.
           , _onLoadAccountData :: Event t AccountId
           }

data Cache t
  = Cache { _families :: Dynamic t Families
          , _invitations :: Dynamic t Invitations
          , _familyInvitations :: Dynamic t FamilyInvitations
          , _accountInvitations :: Dynamict t AccountInvitations
          , _accounts :: Dynamic t Accounts
          , _familyAccounts :: Dynamic t FamilyAccounts
          , _accountFamilies :: Dynamict t AccountFamilies

          , _onLoadedFamilyData :: Event t FamilyId
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



