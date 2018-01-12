{-|
Module      : Gonimo.Server.Clients.ClientStatuses
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Clients.ClientStatus ( ClientStatus(..)
                                          , ClientStatuses
                                          , HasClientStatuses(..)
                                          , clientDeviceStatus
                                          , clientFamily
                                          , makeStatuses
                                          , byFamilyId
                                          , updateStatus
                                          , updateFamily
                                          ) where


import           Control.Lens
import           Data.Default
import           Data.Map                         (Map)
import           Data.Set                         (Set)

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model
import           Gonimo.Prelude


data ClientStatus = ClientStatus { _clientDeviceStatus :: DeviceStatus
                                 , _clientFamily       :: Maybe FamilyId
                                 }

type ClientStatuses = IndexedTable FamilyId Map DeviceId ClientStatus

-- | Update the device status.
--
--   This function won't alter the family of the client and will create the
--   ClientStatus entry with an empty '_clientFamily' if there was no entry
--   before.
updateStatus :: DeviceId -> DeviceStatus -> ClientStatuses -> ClientStatuses
updateStatus devId newStatus = at devId %~ updateStatus'
  where
    getFamily mOld = mOld ^? _Just . clientFamily . _Just
    updateStatus' = Just . ClientStatus newStatus . getFamily

-- | Update the current family of the client.
--
--   '_clientDeviceStatus' won't be altered by this function. If no
--   'ClientStatus' entry with the given 'DeviceId' existed before it will be
--   created with '_clientDeviceStatus' set to a plain 'Online'.
updateFamily :: DeviceId -> Maybe FamilyId -> ClientStatuses -> ClientStatuses
updateFamily devId newFamily = at devId %~ updateFamily'
  where
    getStatus mOld = fromMaybe Online $ mOld ^? _Just . clientDeviceStatus
    updateFamily' = Just . flip ClientStatus newFamily . getStatus


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

