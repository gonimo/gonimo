{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Gonimo.Server.Messenger.Internal where


import           Control.Concurrent.STM    (TVar)
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (StateT (..))
import           Control.Monad.State.Class (MonadState, get, gets)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Data.Set           as Set
import           Data.Maybe (catMaybes)
import           Control.Monad.Extra (whenJust)

import           Gonimo.Db.Entities (DeviceId, FamilyId)
import           Gonimo.Types (DeviceType)

type FromId = DeviceId
type ToId   = DeviceId

data Message
  = MessageSessionGotStolen
  | MessageCreateChannel !FromId !Secret
  | MessageSendMessage !FromId !Secret !Text

data Receiver
  = Receiver { _receiverSend :: !(Message -> IO ())
             , _receiverType :: !DeviceType
             , _receiverFamily :: !(Maybe FamilyId)
             }
$(makeLenses ''Receiver)

data Messenger
  = Messenger { _messengerReceivers :: Map DeviceId Messenger
              , _messengerFamilies :: Map FamilyId (Set DeviceId)
              }
$(makeLenses ''Messenger)


type MessengerVar = TVar Messenger

emptyMessenger :: Messenger
emptyMessenger = Messenger {
    _messengerMessengers = Map.empty
  }

-- | Register a receiver for a given device.
--
--   Any previous receiver will simply be overridden. We steal the session.
registerReceiver :: MonadState Messenger m => DeviceId -> (Message -> IO ()) -> m (Maybe (Message -> IO ()))
registerReceiver deviceId receiver = do
  mOld <- use $ messengerReceivers.at deviceId

  deleteReceiver deviceId -- Delete old one (leave any family!)

  messengerReceivers.at deviceId .= Just $ Receiver receiver NoBaby Nothing
  return mOld

getReceiver :: DeviceId -> Messenger -> Maybe (Message -> IO ())
getReceiver deviceId messenger = messenger^? at deviceId . _Just . receiverSend

getReceiverFamily :: DeviceId -> Messenger -> Maybe (FamilyId)
getReceiverFamily deviceId messenger = messenger^? at deviceId . _Just . receiverFamily


-- | Update a given receiver's device type.
setDeviceType :: MonadState Messenger m => DeviceId -> DeviceType -> m ()
setDeviceType deviceId deviceType = messengerReceivers.at deviceId._Just.receiverType .= deviceType

-- | Retrieve all devices that are online in a given family.
getOnlineDevices :: FamilyId -> Messenger ->  [(DeviceId, DeviceType)]
getOnlineDevices familyId messenger =
  let
    ids = messenger^.at familyId . Set.toList
    getType :: DeviceId -> Maybe DeviceType
    getType deviceId' = messenger.messengerReceivers.at.deviceId'.receiverType
    mTypes = ids %~ mapped getType
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
  messengerReceivers.at deviceId . receiverFamily .= Just newFamily


-- | Delete your online session
--
--   If device id doesn't match - nothing happens.
deleteReceiver :: (MonadState Messenger m) => DeviceId -> m ()
deleteReceiver deviceId = do
  fmap (fromMaybe ()) . runMaybeT $ do
    oldFamily <- MaybeT $ gets (getFamily deviceId)
    lift $ messengerFamilies.at oldFamily . non Set.empty %= Set.delete oldFamily
  sessions.at deviceId .= Nothing


-- Helper functions:
getFamily :: DeviceId -> Messenger -> Maybe FamilyId
getFamily deviceId messenger = messenger^?messengerReceivers.at deviceId._Just.receiverFamily._Just

runStateSTM :: MessengerVar -> State Messenger a -> STM a
runStateSTM mesVar m = do
  messengerOld <- readTVar mesVar
  (val, messengerNew) <- runState m
  writeTVar mesVar messengerNew
  pure val
