module Gonimo.Server.Handlers.Messenger where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Foldable             (traverse_)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)

import           Gonimo.Server.Auth        as Auth
import qualified Gonimo.Server.Db.Device   as Device
import qualified Gonimo.Server.Db.Family   as Family
import           Gonimo.Server.Effects
import           Gonimo.Server.Messenger
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types    hiding (Message, deviceId)
import           Gonimo.Types

-- | A device registers itself (happens at authentication)
--
--   Afterwards you can use switchFamilyR for becoming online in a family.
registerReceiverR :: (HasAuthData env, HasConfig env) => DeviceId -> (Message -> IO ()) -> RIO env ()
registerReceiverR deviceId receiver = do
    authorize =<< isDevice deviceId

    now <- getCurrentTime
    mNotify <- runDb . runMaybeT $ do
      Device.update deviceId $ Device.setLastAccessed now
      pure . notify $ ReqGetDeviceInfo deviceId
    sequence_ mNotify

    fmap (fromMaybe ()) . runMaybeT $ do
      messenger <- getMessenger
      old <- MaybeT . atomically $ registerReceiverSTM messenger deviceId receiver
      liftIO $ old MessageSessionGotStolen

-- | Change device type (become a baby or stop being one)
setDeviceTypeR  :: (HasAuthData env, HasConfig env) => DeviceId -> DeviceType -> RIO env ()
setDeviceTypeR deviceId deviceType = do
    authorize =<< isDevice deviceId

    messenger <- getMessenger
    mFamilyId <- atomically $ do
      setDeviceTypeSTM messenger deviceId deviceType
      getReceiverFamilySTM messenger deviceId
    traverse_ (notify . ReqGetOnlineDevices) mFamilyId

-- | Tell the server that you are gone.
deleteReceiverR  :: (HasAuthData env, HasConfig env) =>  DeviceId -> RIO env ()
deleteReceiverR deviceId = do
  authorize =<< isDevice deviceId

  messenger <- getMessenger
  mFamilyId <- atomically $ do
    mFamilyId' <- getReceiverFamilySTM messenger deviceId
    deleteReceiverSTM messenger deviceId
    pure mFamilyId'

  traverse_ (notify . ReqGetOnlineDevices) mFamilyId

getOnlineDevicesR  :: (HasAuthData env, HasConfig env) => FamilyId -> RIO env [(DeviceId, DeviceType)]
getOnlineDevicesR familyId = do
  authorize =<< isFamilyMember familyId

  messenger <- getMessenger
  atomically $ getOnlineDevicesSTM messenger familyId

switchFamilyR  :: (HasAuthData env, HasConfig env) => DeviceId -> FamilyId -> RIO env ()
switchFamilyR deviceId familyId = do
  authorize =<< (&&) <$> isDevice deviceId <*> isFamilyMember familyId

  messenger <- getMessenger
  mOld <- atomically $ do
    mOld' <- getReceiverFamilySTM messenger deviceId
    switchFamilySTM messenger deviceId familyId
    pure mOld'
  traverse_ (notify . ReqGetOnlineDevices) mOld
  notify $ ReqGetOnlineDevices familyId


-- Update last used baby names and send out notifications on change.
-- TODO: Does not really belong here!
saveBabyNameR :: (HasAuthData env, HasConfig env) => FamilyId -> Text -> RIO env ()
saveBabyNameR familyId babyName = do
  authorize =<< isFamilyMember familyId

  mNotifyFamily <- runDb . runMaybeT $ do
    Family.update familyId (Family.pushBabyName babyName)
    pure . notify $ ReqGetFamily familyId
  sequence_ mNotifyFamily
