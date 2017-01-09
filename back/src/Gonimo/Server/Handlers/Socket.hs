module Gonimo.Server.Handlers.Socket where

import           Data.Text                            (Text)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Server.Effects
import           Gonimo.Server.Error
import           Gonimo.Server.Messenger
import           Gonimo.Types
import           Gonimo.Db.Entities            (FamilyId, DeviceId)
import           Control.Monad.IO.Class (liftIO)

-- | Create a channel for communication with  a baby station
createChannelR :: (AuthReader m, MonadServer m)
              => FamilyId -> DeviceId -> DeviceId -> m Secret
createChannelR familyId fromId toId = do
  authorizeAuthData $ isDevice fromId
  authorizeAuthData $ isFamilyMember familyId

  secret <- generateSecret
  sendMessage familyId toId $ MessageCreateChannel fromId secret
  return secret


sendMessageR :: forall m. (AuthReader m, MonadServer m)
           => FamilyId -> DeviceId -> DeviceId -> Secret -> Text -> m ()
sendMessageR familyId fromId toId secret txt = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((fromId ==) . deviceKey)

    sendMessage familyId toId $ MessageSendMessage fromId secret txt


-- Internal helper function:
sendMessage :: MonadServer m => FamilyId -> DeviceId -> Message -> m ()
sendMessage familyId toId msg = do
  messenger <- getMessenger
  (mFamily, mSend) <- atomically $ do
    mSend' <- getReceiverSTM messenger toId
    mFamily' <- getReceiverFamilySTM messenger toId
    pure (mFamily', mSend')

  authorize (Just familyId ==) mFamily -- Ensure that only devices that are seen as online can send messages!
  case mSend of
    Nothing -> throwServer DeviceOffline
    Just send -> liftIO $ send msg
