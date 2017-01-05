{-# LANGUAGE Unsafe #-}
module Gonimo.Server.Handlers.Socket where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Server.Effects
import           Gonimo.Server.Error
import           Gonimo.Server.State.Types
import           Gonimo.Types
import           Gonimo.WebAPI                        (ReceiveChannelR)
import           Servant.API                          ((:>))
import           Servant.Subscriber                   (Event (ModifyEvent))
import           Control.Monad.Trans.Class            (lift)
import           Gonimo.Db.Entities            (FamilyId, DeviceId)

-- | Create a channel for communication with  a baby station
createChannelR :: (AuthReader m, MonadServer m)
              => FamilyId -> DeviceId -> DeviceId -> m Secret
createChannelR familyId fromId toId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . deviceKey)

  secret <- generateSecret
  sendSessionMessage familyId toId $ SessionCreateChannel fromId secret
  return secret


sendMessageR :: forall m. (AuthReader m, MonadServer m)
           => FamilyId -> DeviceId -> DeviceId -> Secret -> Text -> m ()
sendMessageR familyId fromId toId secret txt = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((fromId ==) . deviceKey)

    sendSessionMessage familyId toId $ SessionSendMessage fromId secret txt


-- Internal helper function:
sendSessionMessage :: MonadServer m => FamilyId -> DeviceId -> SessionMessage -> m ()
sendSessionMessage familyId toId msg = do
  family <- getFamilyEff familyId
  mSend <- family^?familyOnlineStateDevices.to toId._Just.onlineDeviceSend
  case mSend of
    Nothing -> throwServer DeviceOffline
    Just send -> send msg
