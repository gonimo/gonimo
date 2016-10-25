{-# LANGUAGE GADTs #-}
module Gonimo.Server.Effects.Development (
  runExceptionServer
  , Config(..)
  , ServerEffects ) where


import           Control.Concurrent.STM        (atomically, registerDelay)
import           Control.Exception.Base        (SomeException, toException, try)
import           Control.Monad.Freer.Exception (Exc (..), runError)
import           Control.Monad.Freer.Internal  (Arrs, Eff (..), decomp, qApp)
import           Control.Monad.Logger          (ToLogStr (..))
import           Crypto.Random                 (SystemRandom, genBytes,
                                                newGenIO)
import           Data.Bifunctor
import           Data.ByteString.Lazy                    (toStrict)
import           Data.Text                               (Text)
import qualified Data.Text.IO                            as T
import qualified Data.Text.Encoding                      as T
import           Network.Mail.Mime
import           Data.Time.Clock                         (getCurrentTime)
import           Servant.Subscriber
import           System.Random                           (getStdRandom)

import           Gonimo.Server.Effects.Common
import           Gonimo.Server.Effects.Internal


runExceptionServer :: Config -> Eff (Exc SomeException ': '[Server]) w  -> IO (Either SomeException w)
runExceptionServer c = runServer c . runError

-- runTimeoutServer :: Config -> Int -> Eff (Exc SomeException ': '[Server]) w -> IO (Either SomeException w)
-- runTimeoutServer c timeout action = do
--   r <- runExceptionServer c action


runServer :: forall w . Config -> Eff '[Server] (Either SomeException w) -> IO (Either SomeException w)
runServer _ (Val v) = return v
runServer c (E u' q) = case decomp u' of
  Right (Atomically m)             -> execIO c q $ atomically m
  Right (SendEmail mail)           -> execIO c q $ T.putStrLn $ getMailBody mail
  Right (LogMessage loc ls ll msg) -> execIO c q $ doLog loc ls ll (toLogStr msg)
  Right (GenRandomBytes l)         ->
    -- Throw away the new generator & make an exception of any occurred error:
    bimap toException fst . genBytes l <$> (newGenIO :: IO SystemRandom)
    >>= runServer c . qApp q
  -- Right (Timeout _ eff)            -> runExceptionServer c eff >>= runServer c . qApp q
  Right (RegisterDelay n)          -> execIO c q $ registerDelay n
  Right GetCurrentTime             -> execIO c q getCurrentTime
  Right GetState                   -> runServer c . qApp q $ Right (state c)
  Right (Notify ev pE cB)          -> execIO c q $ atomically (notify (subscriber c) ev pE cB)
  Right (RunDb trans)              -> runDatabaseServerIO pool trans >>= runServer c . qApp q
  Right (RunRandom rand)           -> execIO c q (getStdRandom rand)
  Left  _                          -> error impossibleMessage
  where
    pool = configPool c
    doLog = configLog c
    -- runLogger loggerT = runLoggingT loggerT doLog


execIO :: Config
          -> Arrs '[Server] (Either SomeException b) (Either SomeException w)
          -> IO b
          -> IO (Either SomeException w)
execIO c q action = try action >>= runServer c . qApp q


getMailBody :: Mail -> Text
getMailBody mail = let
    parts = concat $ mailParts mail
    textParts = filter ((== "text/plain; charset=utf-8") . partType) parts
    textPart = case textParts of
                 (x : _) -> partContent x
                 _ -> "!Mail contained no text!"
  in
      T.decodeUtf8 . toStrict $ textPart
