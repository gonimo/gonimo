module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerConstraint
  , ServerEffects
  , sendServer
  ) where

import Control.Exception.Base (SomeException)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Control.Monad.Logger (Loc, LogLevel, LogSource, ToLogStr)
import Data.ByteString (ByteString)

import Data.Time.Clock (UTCTime)

import Network.Mail.Mime (Mail)
import Gonimo.Database.Effects
import Database.Persist.Sql (SqlBackend)

-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc SomeException) r)
type ServerEffects = Eff '[Exc SomeException, Server]


-- Tidy up the following Server definition
-- We need the ability to handle exceptions in the interpreted code at least for proper handling
-- of database transactions:
type EServer a =  Server (Either SomeException a)

data Server v where
  SendEmail :: !Mail -> EServer ()
  LogMessage :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> EServer ()
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  RunDb :: Eff '[Exc SomeException, Database SqlBackend]  a -> EServer a

-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v
