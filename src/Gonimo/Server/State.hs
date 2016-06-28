{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.State where


import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding (to, from)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Set                    (Set)

import           Gonimo.Server.Types (Secret)
import           Gonimo.Server.DbEntities (FamilyId, ClientId)

newtype SecretDB = SecretDB { fetch :: Map ClientId (ClientId, Secret)}

data Transient = Transient { _secrets :: SecretDB
                           , _online  :: Set ClientId }
$(makeLenses ''Transient)

type FamilyMap = Map FamilyId Transient

data State = State { _runState :: TVar FamilyMap }
$(makeLenses ''State)

putSecret :: ClientId -> ClientId -> Secret -> SecretDB -> SecretDB
-- | putSecret inserts possibly overwrites
putSecret from to secret = SecretDB . M.insert to (from, secret) . fetch

getSecret :: ClientId -> SecretDB -> Maybe (ClientId, Secret)
getSecret to = M.lookup to . fetch

deleteSecret :: ClientId -> SecretDB -> SecretDB
deleteSecret to = SecretDB . M.delete to . fetch

