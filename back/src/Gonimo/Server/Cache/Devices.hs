{-|
Module      : Gonimo.Server.Cache.Devices
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.Devices where

import           Data.Map                         (Map)
import           Data.Set                         (Set)

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model


type Devices = IndexedTable AccountId Map DeviceId Device

-- | Create a new Devices indexed data structure from a raw Map
make :: Map DeviceId Device -> Devices
make devices' = fromRawTable (Just . deviceAccountId) devices'


-- | Search entries by Secret
byAccountId :: Devices -> Map AccountId (Set DeviceId)
byAccountId = getIndex
