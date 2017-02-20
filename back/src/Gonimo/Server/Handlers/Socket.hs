module Gonimo.Server.Handlers.Socket where

import           Control.Monad.IO.Class  (liftIO)
import           Gonimo.Db.Entities      (DeviceId)
import           Gonimo.Server.Auth      as Auth
import           Gonimo.Server.Effects
import           Gonimo.Server.Error
import           Gonimo.Server.Messenger
import qualified Gonimo.SocketAPI.Types  as API
import           Gonimo.Types

-- | Create a channel for communication with  a baby station
createChannelR :: (AuthReader m, MonadServer m)
              => DeviceId -> DeviceId -> m Secret
createChannelR fromId toId = do
  secret <- generateSecret
  sendMessage fromId toId $ MessageCreateChannel fromId secret
  return secret


sendMessageR :: forall m. (AuthReader m, MonadServer m)
           => DeviceId -> DeviceId -> Secret -> API.Message -> m ()
sendMessageR fromId toId secret msg
  = sendMessage fromId toId $ MessageSendMessage fromId secret msg


-- Internal helper function:
sendMessage :: (AuthReader m, MonadServer m) => DeviceId -> DeviceId -> Message -> m ()
sendMessage fromId toId msg = do
  authorizeAuthData $ isDevice fromId

  messenger <- getMessenger
  (mFromFamily, mToFamily, mSend) <- atomically $ do
    mFromFamily' <- getReceiverFamilySTM messenger fromId
    mToFamily' <- getReceiverFamilySTM messenger toId

    mSend' <- getReceiverSTM messenger toId
    pure (mFromFamily', mToFamily', mSend')

  -- Both devices have to be online:
  fromFamily <- authorizeJust id mFromFamily
  toFamily   <- authorizeJust id mToFamily
  -- and online in the same family:
  authorize (fromFamily ==) toFamily
  -- Additional sanity check:
  authorizeAuthData $ isFamilyMember fromFamily
  case mSend of
    Nothing -> throwServer DeviceOffline
    Just send -> liftIO $ send msg
