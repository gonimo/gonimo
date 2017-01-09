module Gonimo.Server.Messenger ( module I
                               , registerReceiverSTM
                               , getReceiverSTM
                               , getReceiverFamilySTM
                               , setDeviceTypeSTM
                               , getOnlineDevicesSTM
                               , deleteReceiverSTM
                               , switchFamilySTM
                               ) where



import           Gonimo.Db.Entities (DeviceId)
import           Gonimo.Types      (DeviceType)
import           Control.Concurrent.STM (STM)
import           Control.Concurrent.STM.TVar (readTVar)
import           Gonimo.Db.Entities


import Gonimo.Server.Messenger.Internal as I ( FromId
                                             , ToId
                                             , Message(..)
                                             , Messenger
                                             , MessengerVar
                                             , empty
                                             )

import Gonimo.Server.Messenger.Internal

registerReceiverSTM :: MessengerVar -> DeviceId -> (Message -> IO ()) -> STM (Maybe (Message -> IO ()))
registerReceiverSTM mesVar deviceId receiver = runStateSTM mesVar $ registerReceiver deviceId receiver

setDeviceTypeSTM :: MessengerVar -> DeviceId -> DeviceType -> STM ()
setDeviceTypeSTM mesVar deviceId deviceType = runStateSTM mesVar $ setDeviceType deviceId deviceType

getOnlineDevicesSTM :: MessengerVar -> FamilyId -> STM [(DeviceId, DeviceType)]
getOnlineDevicesSTM mesVar familyId = getOnlineDevices familyId <$> readTVar mesVar

getReceiverSTM :: MessengerVar -> DeviceId ->  STM (Maybe (Message -> IO ()))
getReceiverSTM mesVar deviceId = getReceiver deviceId <$> readTVar mesVar

getReceiverFamilySTM :: MessengerVar -> DeviceId ->  STM (Maybe FamilyId)
getReceiverFamilySTM mesVar deviceId = getReceiverFamily deviceId <$> readTVar mesVar

switchFamilySTM :: MessengerVar -> DeviceId -> FamilyId -> STM ()
switchFamilySTM mesVar deviceId familyId = runStateSTM mesVar $ switchFamily deviceId familyId

deleteReceiverSTM :: MessengerVar -> DeviceId -> STM ()
deleteReceiverSTM mesVar = runStateSTM mesVar . deleteReceiver
