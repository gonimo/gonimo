module Gonimo.Server.Handlers.Socket where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Catch  as X (MonadThrow (..))

import           Gonimo.Server.Auth      as Auth
import           Gonimo.Server.Config
import           Gonimo.Server.Error
import           Gonimo.Server.Messenger
import           Gonimo.SocketAPI.Types  (DeviceId)
import qualified Gonimo.SocketAPI.Types  as API
import           Gonimo.Types

-- | Create a channel for communication with  a baby station
createChannelR :: (HasAuthData env, HasConfig env)
  => DeviceId -> DeviceId -> RIO env Secret
createChannelR fromId toId = do
  secret <- generateSecret
  sendMessage fromId toId $ MessageCreateChannel fromId secret
  return secret


sendMessageR :: forall env. (HasAuthData env, HasConfig env)
  => DeviceId -> DeviceId -> Secret -> API.Message -> RIO env ()
sendMessageR fromId toId secret msg
  = sendMessage fromId toId $ MessageSendMessage fromId secret msg


-- Internal helper function:
sendMessage :: (HasAuthData env, HasConfig env) => DeviceId -> DeviceId -> Message -> RIO env ()
sendMessage fromId toId msg = do
  authorize =<< isDevice fromId

  messenger <- getMessenger
  (mFromFamily, mToFamily, mSend) <- atomically $ do
    mFromFamily' <- getReceiverFamilySTM messenger fromId
    mToFamily' <- getReceiverFamilySTM messenger toId

    mSend' <- getReceiverSTM messenger toId
    pure (mFromFamily', mToFamily', mSend')

  -- Both devices have to be online:
  fromFamily <- authorizeJust mFromFamily
  toFamily   <- authorizeJust mToFamily
  -- and online in the same family:
  authorize $ fromFamily == toFamily
  -- Additional sanity check:
  authorize =<< isFamilyMember fromFamily
  case mSend of
    Nothing -> throwM DeviceOffline
    Just send -> liftIO $ send msg
