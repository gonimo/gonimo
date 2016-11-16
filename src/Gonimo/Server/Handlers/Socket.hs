{-# LANGUAGE Unsafe #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.Handlers.Socket where

import           Data.Monoid
import           Control.Applicative                  ((<|>))
import           Control.Monad.Freer                  (Eff)
import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import           Gonimo.Server.Auth                   as Auth
import           Gonimo.Server.Effects
import           Gonimo.Server.Error
import qualified Gonimo.Server.State.MessageBox       as MsgBox
import           Gonimo.Server.State.Types
import           Gonimo.Server.Types
import           Gonimo.WebAPI                        (ReceiveChannelR,
                                                       ReceiveMessageR)
import           Servant.API                          ((:>))
import           Servant.Subscriber                   (Event (ModifyEvent))
import           Control.Monad.Trans.Class            (lift)
import           Gonimo.Server.Db.Entities            (FamilyId, DeviceId)
import           Unsafe.Coerce
import qualified Data.Text                            as T
import           Database.Persist.Class

-- | Create a channel for communication with  a baby station
--
--   The baby station must call receiveChannel within a given timeout,
--   this handler will only return a secret if the baby station did so,
--   otherwise an error is thrown (not found - `NoSuchSocket`)
createChannel :: AuthServerConstraint r
              => FamilyId -> DeviceId -> DeviceId -> Eff r Secret
createChannel familyId toId fromId = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((fromId ==) . deviceKey)

  secret <- generateSecret
  $(logDebug) $ "SOCKET: create (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): waiting for socket to become clear ...."
  updateFamilyRetryEff SocketBusy familyId $ MsgBox.setData channelSecrets toId (fromId, secret)

  $(logDebug) $ "SOCKET: create (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): notifying reader ...."
  notify ModifyEvent endpoint (\f -> f familyId toId)

  $(logDebug) $ "SOCKET: create (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): waiting for reader...."
  waitForReaderEff NoSuchSocket familyId toId channelSecrets
  $(logDebug) $ "SOCKET: create (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): quitting ...."
  return secret
 where
   endpoint :: Proxy ("socket" :> ReceiveChannelR)
   endpoint = Proxy


receiveChannel :: AuthServerConstraint r
               => FamilyId -> DeviceId -> Eff r (Maybe ChannelRequest)
-- in this request @to@ is the one receiving the secret
receiveChannel familyId toId = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((toId ==) . deviceKey)

    $(logDebug) $ "SOCKET: receiveChannel (" <> prettyKey toId <>"): getting channel key ...."
    fmap fst . MsgBox.getData toId . _channelSecrets <$> getFamilyEff familyId

deleteChannelRequest :: AuthServerConstraint r
                        =>  FamilyId -> DeviceId -> DeviceId -> Secret -> Eff r ()
deleteChannelRequest familyId toId fromId secret = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((toId ==) . deviceKey)

    $(logDebug) $ "SOCKET: delete (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): marking as read ...."
    let chanRequest = (fromId, secret)
    _ :: Maybe () <- runMaybeT $ do
      let markRead = mayUpdateFamilyEff familyId $ MsgBox.markRead channelSecrets toId chanRequest
      markRead <|> lift (throwServer (ChannelAlreadyGone chanRequest))
    pure ()

putMessage :: forall r. AuthServerConstraint r
           => FamilyId -> DeviceId -> DeviceId -> Secret -> Text -> Eff r ()
putMessage familyId fromId toId secret txt = do
    authorizeAuthData (isFamilyMember familyId)
    authorizeAuthData ((fromId ==) . deviceKey)

    let key = (fromId, toId, secret)
    $(logDebug) $ "SOCKET: putMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): waiting for channel ...."
    updateFamilyRetryEff ChannelBusy familyId $ MsgBox.setData channelData key txt

    $(logDebug) $ "SOCKET: putMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): notifying reader ...."
    notify ModifyEvent endpoint (\f -> f familyId fromId toId secret)

    $(logDebug) $ "SOCKET: putMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): waiting for reader ...."
    waitForReaderEff NoSuchChannel familyId key channelData
    $(logDebug) $ "SOCKET: putMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): quitting ...."
  where

   endpoint :: Proxy ("socket" :> ReceiveMessageR)
   endpoint = Proxy


receiveMessage :: AuthServerConstraint r
               => FamilyId -> DeviceId -> DeviceId -> Secret -> Eff r (Maybe (MessageNumber, Text))
receiveMessage familyId fromId toId secret = do
  authorizeAuthData (isFamilyMember familyId)
  authorizeAuthData ((toId ==) . deviceKey)

  $(logDebug) $ "SOCKET: receiveMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <>"): reading data ...."
  MsgBox.getData (fromId, toId, secret) . _channelData <$> getFamilyEff familyId

deleteMessage :: AuthServerConstraint r => FamilyId -> DeviceId -> DeviceId -> Secret
                -> MessageNumber -> Eff r ()
deleteMessage familyId fromId toId secret num = do
    let key = (fromId, toId, secret)
    _ :: Maybe () <-runMaybeT $ do
      let markRead = mayUpdateFamilyEff familyId $ MsgBox.markRead channelData key num
      lift $ $(logDebug) $ "SOCKET: deleteMessage (" <> prettyKey toId <> ", " <> prettyKey fromId <> ", " <> (T.pack . show $ secret) <> "," <> (T.pack . show $ num ) <> "): marking as read ...."
      markRead <|> lift (throwServer (MessageAlreadyGone num))
    pure ()

-- Internal function for debugging:
prettyKey :: Key a -> Text
prettyKey = T.pack . show . (unsafeCoerce :: Key a -> Int)
