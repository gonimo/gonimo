{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Gonimo.Server.State where


import           Control.Concurrent.STM
import           Control.Monad               (forever)
import           Control.Lens                hiding (to, from)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Set                    (Set)

import           Gonimo.Server.Types (Secret)
import           Gonimo.Server.DbEntities (FamilyId, ClientId)

type ChannelDB = Map ClientId (ClientId, Secret)

data Transient = Transient { _secrets :: ChannelDB
                           , _online  :: Set ClientId }
$(makeLenses ''Transient)

type FamilyMap = Map FamilyId (TVar Transient)

data State = State { _runState :: TVar FamilyMap }
$(makeLenses ''State)

putSecret :: ClientId -> ClientId -> Secret -> TVar Transient -> STM ()
-- | putSecret inserts possibly overwrites
putSecret from to secret transient = forever $ do
  t <- _secrets <$> readTVar transient
  check (to `M.member` t)
  modifyTVar transient (secrets %~ to `M.insert` (from, secret))

receieveSecret :: ClientId -> TVar Transient -> STM (ClientId, Secret)
receieveSecret to transient = -- M.lookup to . fetch
                    forever $ do
  t <- _secrets <$> readTVar transient
  case to `M.lookup` t of
    Nothing ->  receieveSecret to transient
    Just cs -> do modifyTVar transient (secrets %~ M.delete to)
                  return cs
  
  --modifyTVar transient (secrets %~ M.insert to (from, secret))

deleteSecret :: ClientId -> ChannelDB -> ChannelDB
deleteSecret to = undefined -- ChannelDB . M.delete to . fetch

