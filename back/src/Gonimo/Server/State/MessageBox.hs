{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.State.MessageBox where


import           Control.Lens
import           Control.Monad             (MonadPlus, guard)
import           Control.Monad.State.Class
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (isJust)
import           Gonimo.Server.State.Types (MessageBox (..),
                                            messageNumber,
                                            messageState,
                                            MessageState (..),
                                             _Read, _Written, GetMessageNumber,
                                             getMessageNumber)


type MessageBoxes key val num = Map key (MessageBox num val)

setData :: (MonadState state m, MonadPlus m, Ord key, GetMessageNumber state val num)
           => Lens' state (MessageBoxes key val num) -> key -> val -> m ()
setData messageBoxes key val = do
  boxes <- use messageBoxes
  guard (not $ key `M.member` boxes)
  next <- getMessageNumber val
  messageBoxes.at key .= Just (MessageBox next (Written val))

clearIfRead :: (MonadState state m, MonadPlus m, Ord key)
           => Lens' state (MessageBoxes key val num) -> key -> m ()
clearIfRead messageBoxes key = do
  guard =<< use (messageBoxes.to (isRead key))
  messageBoxes.at key .= Nothing

clearData :: (MonadState state m, Ord key)
           => Lens' state (MessageBoxes key val num) -> key -> m ()
clearData messageBoxes key = messageBoxes.at key .= Nothing

getData :: Ord key => key -> MessageBoxes key val num -> Maybe (num, val)
getData key messageBoxes =
  let
    mNum = messageBoxes^?at key._Just.messageNumber
    mVal = messageBoxes^?at key._Just.messageState._Written
  in
    (,) <$> mNum <*> mVal

-- | A reader does not clear the data itself, it just marks it as read.
--   This is to avoid race conditions between writers.
markRead :: (MonadState state m, MonadPlus m, Ord key, Eq num)
           => Lens' state (MessageBoxes key val num) -> key -> num -> m ()
markRead messageBoxes key number = do
  box <- use (messageBoxes.at key)
  guard (isJust (box^?_Just.messageState._Written) && (box^?_Just.messageNumber == Just number))
  messageBoxes.at key._Just.messageState .= Read

isRead :: Ord key => key -> MessageBoxes key val num -> Bool
isRead key messageBoxes = isJust $ messageBoxes^?at key._Just.messageState._Read
