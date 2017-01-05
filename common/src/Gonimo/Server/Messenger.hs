module Gonimo.Server.Messenger ( module I
                               , registerReceiverSTM
                               , getReceiverSTM
                               , getReceiverFamilySTM
                               , setDeviceTypeSTM
                               , getOnlineDevicesSTM
                               , switchFamilyStM
                               , deleteReceiverSTM
                               ) where


import           Control.Lens
import           Control.Monad             (unless, mzero, MonadPlus, guard)
import qualified Data.Map.Strict           as M
import           Control.Monad.Error.Class
import           Control.Monad.Trans.State (runState, State)

import           Gonimo.Db.Entities (DeviceId)
import           Gonimo.Server.Error      (ServerError (NoActiveSession, SessionInvalid),
                                           ToServerError, toServerError)
import           Gonimo.Types      (DeviceType)
import           Gonimo.Server.State.Types (Online, sessions, idCounter, SessionId(..))
import           Control.Monad.Extra (whenJust)
import           Data.Maybe (catMaybes)


import Gonimo.Server.Messenger.Internal as I ( FromId
                                             , ToId
                                             , Message(..)
                                             , Messenger
                                             , MessengerVar
                                             , emptyMessenger
                                             )

import Gonimo.Server.Messenger.Internal

registerReceiverSTM :: MessengerVar -> DeviceId -> (Message -> IO ()) -> STM (Maybe (Message -> IO ()))
registerReceiverSTM mesVar deviceId receiver = runStateSTM mesVar $ registerReceiver deviceId receiver

setDeviceTypeSTM :: MessengerVar -> DeviceId -> DeviceType -> STM ()
setDeviceTypeSTM mesVar deviceId deviceType = runStateSTM mesVar $ setDeviceType deviceId deviceType

getOnlineDevicesSTM :: MessengerVar -> FamilyId ->  [(DeviceId, DeviceType)]
getOnlineDevicesSTM mesVar familyId = getOnlineDevices familyId <$> readTVar mesVar

getReceiverSTM :: MessengerVar -> DeviceId ->  Maybe (Message -> IO ())
getReceiverSTM mesVar deviceId = getReceiver deviceId <$> readTVar mesVar

getReceiverFamilySTM :: MessengerVar -> DeviceId ->  Maybe Family
getReceiverFamilySTM mesVar deviceId = getReceiverFamily deviceId <$> readTVar mesVar

switchFamilySTM :: MessengerVar -> DeviceId -> FamilyId -> STM ()
switchFamilySTM mesVar deviceId familyId = runStateSTM mesVar $ switchFamily deviceId familyId

deleteReceiverSTM :: MessengerVar -> DeviceId -> STM ()
deleteReceiverSTM mesVar = runStateSTM mesVar . deleteReceiver
