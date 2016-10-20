{-# LANGUAGE GADTs #-}
module Gonimo.Server.Effects.Production (
  runExceptionServer
  , Config(..)
  , ServerEffects ) where


import           Control.Exception.Base                  (SomeException, toException, try)
import           Control.Monad.Freer.Exception           (Exc (..), runError)
import           Control.Monad.Freer.Internal            (Arrs, Eff (..),
                                                          decomp, qApp)
import           Control.Monad.Logger                    (ToLogStr (..))
import           Crypto.Random                           (SystemRandom,
                                                          genBytes, newGenIO)
import           Data.Bifunctor
import           Data.Time.Clock                         (getCurrentTime)
import           Network.Mail.SMTP                       (sendMail)
import           Servant.Subscriber
import           Control.Concurrent.STM (atomically)

import           Gonimo.Server.Effects.Internal
import           Gonimo.Server.Effects.Common

import           System.Random                           (getStdRandom)


runExceptionServer :: Config -> Eff (Exc SomeException ': '[Server]) w  -> IO (Either SomeException w)
runExceptionServer c = runServer c . runError



runServer :: forall w . Config -> Eff '[Server] (Either SomeException w) -> IO (Either SomeException w)
runServer _ (Val v) = return v
runServer c (E u' q) = case decomp u' of
  Right (Atomically m)             -> execIO c q $ atomically m
  Right (SendEmail mail)           -> execIO c q $ sendMail "localhost" mail
  Right (LogMessage loc ls ll msg) -> execIO c q $ doLog loc ls ll (toLogStr msg)
  Right (GenRandomBytes l)         ->
    -- Throw away the new generator & make an exception of any occurred error:
    bimap toException fst . genBytes l <$> (newGenIO :: IO SystemRandom)
    >>= runServer c . qApp q
  Right (Timeout _ eff)            -> runExceptionServer c eff >>= runServer c . qApp q
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
