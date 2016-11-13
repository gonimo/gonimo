{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.State.MessageBox where


import           Control.Lens
import           Control.Monad             (MonadPlus, guard)
import           Control.Monad.State.Class
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (isJust)
import           Gonimo.Server.State.Types (FamilyOnlineState, MessageBox (..),
                                            MessageNumber (..),
                                            MessageState (..), channelSequence,
                                            messageState, _Read,
                                            _Written, _MessageBox)


type MessageBoxes key val = Map key (MessageBox val)

setData :: (MonadState FamilyOnlineState m, Monad m, MonadPlus m, Ord key)
           => Lens' FamilyOnlineState (MessageBoxes key val) -> key -> val -> m ()
setData messageBoxes key val = do
  boxes <- use messageBoxes
  guard (not $ key `M.member` boxes)
  next <- getNewMessageNumber
  messageBoxes.at key .= Just (MessageBox next (Written val))

clearIfRead :: (MonadState state m, Monad m, MonadPlus m, Ord key)
           => Lens' state (MessageBoxes key val) -> key -> m ()
clearIfRead messageBoxes key = do
  guard =<< use (messageBoxes.to (isRead key))
  messageBoxes.at key .= Nothing

clearData :: (MonadState state m, Monad m, Ord key)
           => Lens' state (MessageBoxes key val) -> key -> m ()
clearData messageBoxes key = messageBoxes.at key .= Nothing

getData :: Ord key => key -> MessageBoxes key val -> Maybe (MessageNumber, val)
getData key messageBoxes =
  let
    mNum = messageBoxes^.at key._Just.messageNumber
    mVal = messageBoxes^?at key._Just.messageState._Written
  in
    (,) <$> mNum mVal

-- | A reader does not clear the data itself, it just marks it as read.
--   This is to avoid race conditions between writers.
markRead :: (MonadState state m, Monad m, MonadPlus m, Ord key)
           => Lens' state (MessageBoxes key val) -> key -> MessageNumber -> m ()
markRead messageBoxes key number= do
  boxes <- use messageBoxes
  box <- use (at key)
  guard (isJust (box^?_Just.messageState._Written) && (box^._Just.messageNumber == number))
  messageBoxes.at key.messageState ?= Read

isRead :: Ord key => key -> MessageBoxes key val -> Bool
isRead key messageBoxes = isJust $ messageBoxes^?at key._Just._Read


-- Internal helper function:

getNewMessageNumber :: MonadState FamilyOnlineState m => m MessageNumber
getNewMessageNumber = do
  newId <- use channelSequence
  channelSequence += 1
  pure $ MessageNumber newId
