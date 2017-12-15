{-|
Module      : Gonimo.Server.Clients.ClientStatuses
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.ClientStatus ( ClientStatus(..)
                                          , ClientStatuses
                                          , clientDeviceStatus
                                          , clientFamily
                                          , make
                                          , byFamilyId
                                          ) where


import           Data.Map                         (Map)
import Control.Lens

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model


data ClientStatus = ClientStatus { _clientDeviceStatus :: DeviceStatus
                                 , _clientFamily :: Maybe FamilyId
                                 }

type ClientStatuses = IndexedTable FamilyId Map DeviceId ClientStatus

-- | Create a new ClientStatuses indexed data structure from a raw Map
make :: Map DeviceId ClientStatus -> ClientStatuses
make clientStatuses' = fromRawTable _clientFamily clientStatuses'


-- | Serch entries by FamilyId
byFamilyId :: ClientStatuses -> Map FamilyId [DeviceId]
byFamilyId = getIndex

-- Lenses for ClientStatus:

clientDeviceStatus :: Lens' ClientStatus DeviceStatus
clientDeviceStatus f clientStatus' = (\clientDeviceStatus' -> clientStatus' { _clientDeviceStatus = clientDeviceStatus' }) <$> f (_clientDeviceStatus clientStatus')

clientFamily :: Lens' ClientStatus (Maybe FamilyId)
clientFamily f clientStatus' = (\clientFamily' -> clientStatus' { _clientFamily = clientFamily' }) <$> f (_clientFamily clientStatus')


