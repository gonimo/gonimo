{-|
Module      : Gonimo.Server.Clients.ClientStatuses
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.ClientStatus ( ClientStatus(..)
                                          , ClientStatuses
                                          , clientDeviceStatus
                                          , clientFamily
                                          , makeStatuses
                                          , byFamilyId
                                          ) where


import           Control.Lens
import           Data.Default
import           Data.Map                         (Map)
import           Data.Set                         (Set)

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model


data ClientStatus = ClientStatus { _clientDeviceStatus :: DeviceStatus
                                 , _clientFamily       :: Maybe FamilyId
                                 }

type ClientStatuses = IndexedTable FamilyId Map DeviceId ClientStatus


instance Default ClientStatus where
  def = ClientStatus Online Nothing

-- | Create a new ClientStatuses indexed data structure from a raw Map
makeStatuses :: Map DeviceId ClientStatus -> ClientStatuses
makeStatuses clientStatuses' = fromRawTable _clientFamily clientStatuses'


-- | Serch entries by FamilyId
byFamilyId :: ClientStatuses -> Map FamilyId (Set DeviceId)
byFamilyId = getIndex

-- Manual lens stuff:


-- | A type that contains clientStatuses ...
class HasClientStatuses a where
  clientStatuses :: Lens' a ClientStatuses

-- Lenses for ClientStatus:

clientDeviceStatus :: Lens' ClientStatus DeviceStatus
clientDeviceStatus f clientStatus' = (\clientDeviceStatus' -> clientStatus' { _clientDeviceStatus = clientDeviceStatus' }) <$> f (_clientDeviceStatus clientStatus')

clientFamily :: Lens' ClientStatus (Maybe FamilyId)
clientFamily f clientStatus' = (\clientFamily' -> clientStatus' { _clientFamily = clientFamily' }) <$> f (_clientFamily clientStatus')

