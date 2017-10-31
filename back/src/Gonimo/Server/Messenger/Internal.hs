{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Gonimo.Server.Messenger.Internal where


import           Control.Concurrent.STM    (TVar, readTVar, writeTVar)
import           Control.Lens
import           Control.Monad.State.Class (MonadState, gets)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (State, runState)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Control.Concurrent.STM    (STM)
import           Gonimo.SocketAPI.Types    hiding (FromId, ToId, Message, deviceId)
import qualified Gonimo.SocketAPI.Types    as API
import           Gonimo.Types              (DeviceType (..), Secret)

type FromId = DeviceId
type ToId   = DeviceId

data Message
  = MessageSessionGotStolen
  | MessageCreateChannel !FromId !Secret
  | MessageSendMessage !FromId !Secret API.Message

data Receiver
  = Receiver { _receiverSend :: !(Message -> IO ())
             , _receiverType :: !DeviceType
             , _receiverFamily :: !(Maybe FamilyId)
             }
$(makeLenses ''Receiver)

data Messenger
  = Messenger { _messengerReceivers :: Map DeviceId Receiver
              , _messengerFamilies :: Map FamilyId (Set DeviceId)
              }
$(makeLenses ''Messenger)


type MessengerVar = TVar Messenger

empty :: Messenger
empty = Messenger {
    _messengerReceivers = Map.empty
  , _messengerFamilies = Map.empty
  }

-- | Register a receiver for a given device.
--
--   Any previous receiver will simply be overridden. We steal the session.
registerReceiver :: MonadState Messenger m => DeviceId -> (Message -> IO ()) -> m (Maybe (Message -> IO ()))
registerReceiver deviceId receiver = do
  mOld <- use $ messengerReceivers.at deviceId

  deleteReceiver deviceId -- Delete old one (leave any family!)

  messengerReceivers.at deviceId .= Just (Receiver receiver NoBaby Nothing)
  return $ mOld^?_Just.receiverSend

getReceiver :: DeviceId -> Messenger -> Maybe (Message -> IO ())
getReceiver deviceId messenger = messenger^? messengerReceivers . at deviceId . _Just . receiverSend

getReceiverFamily :: DeviceId -> Messenger -> Maybe FamilyId
getReceiverFamily deviceId messenger = messenger^? messengerReceivers
                                       .  at deviceId . _Just . receiverFamily . _Just


-- | Update a given receiver's device type.
setDeviceType :: MonadState Messenger m => DeviceId -> DeviceType -> m ()
setDeviceType deviceId deviceType = messengerReceivers.at deviceId._Just.receiverType .= deviceType

-- | Retrieve all devices that are online in a given family.
getOnlineDevices :: FamilyId -> Messenger -> [(DeviceId, DeviceType)]
getOnlineDevices familyId messenger =
  let
    ids = messenger^.messengerFamilies . at familyId . non Set.empty . to Set.toList
    getType :: DeviceId -> Maybe DeviceType
    getType deviceId' = messenger^?messengerReceivers.at deviceId'._Just.receiverType
    mTypes = fmap getType ids
    mList = zipWith (\id' mType -> (,) <$> pure id' <*> mType) ids mTypes
  in
    catMaybes mList

switchFamily :: MonadState Messenger m => DeviceId -> FamilyId -> m ()
switchFamily deviceId newFamily = do
  -- Delete old:
  fmap (fromMaybe ()) . runMaybeT $ do
    oldFamily <- MaybeT $ gets (getFamily deviceId)
    lift $ messengerFamilies.at oldFamily . non Set.empty %= Set.delete deviceId

  -- Set new:
  messengerFamilies.at newFamily . non Set.empty %= Set.insert deviceId
  messengerReceivers.at deviceId . _Just . receiverFamily .= Just newFamily


-- | Delete your online session
--
--   If device id doesn't match - nothing happens.
deleteReceiver :: (MonadState Messenger m) => DeviceId -> m ()
deleteReceiver deviceId = do
  fmap (fromMaybe ()) . runMaybeT $ do
    oldFamily <- MaybeT $ gets (getFamily deviceId)
    lift $ messengerFamilies.at oldFamily . non Set.empty %= Set.delete deviceId
  messengerReceivers.at deviceId .= Nothing


-- Helper functions:
getFamily :: DeviceId -> Messenger -> Maybe FamilyId
getFamily deviceId messenger = messenger^?messengerReceivers.at deviceId._Just.receiverFamily._Just

runStateSTM :: MessengerVar -> State Messenger a -> STM a
runStateSTM mesVar m = do
  messengerOld <- readTVar mesVar
  let (val, messengerNew) = runState m messengerOld
  writeTVar mesVar messengerNew
  pure val
