module Utils.STM where

import           Control.Concurrent.STM
import           Control.Exception
import           Gonimo.Server.Error    (ServerError (TransactionTimeout),
                                         makeServantErr)
import           Utils.Constants

gTimeoutSTM :: Exception e => Int -> STM (Maybe a) -> e -> IO a
-- | retries to execute an 'STM' action for for some specified 'Microseconds'
gTimeoutSTM μs action ex = do
  isDelayOver <- registerDelay μs
  atomically $ do isOver <- readTVar isDelayOver
                  if isOver
                     then throwSTM ex
                     else do result <- action
                             maybe retry {-or-} return result

timeoutSTM :: Microseconds -> STM (Maybe a) -> IO a
timeoutSTM μs action = gTimeoutSTM μs action (makeServantErr TransactionTimeout)
