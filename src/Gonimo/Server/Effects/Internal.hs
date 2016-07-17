{-# LANGUAGE GADTs #-}
module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerConstraint
  , ServerEffects
  , sendServer
  ) where

import           Control.Concurrent.STM        (STM)
import           Control.Exception.Base        (SomeException)
import           Control.Monad.Freer           (Eff, Member, send)
import           Control.Monad.Freer.Exception (Exc (..), throwError)
import           Control.Monad.Logger          (Loc, LogLevel, LogSource,
                                                ToLogStr)
import           Data.ByteString               (ByteString)
import           Data.Proxy                    (Proxy (..))
import           Data.Time.Clock               (UTCTime)
import           Database.Persist.Sql          (SqlBackend)
import           Network.Mail.Mime             (Mail)
import           Servant.Subscriber            (Event, HasLink, IsElem,
                                                IsSubscribable, IsValidEndpoint,
                                                MkLink, URI)

import           Gonimo.Database.Effects
import           Gonimo.Server.State           (OnlineState)
import           Gonimo.WebAPI                 (GonimoAPI)

-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc SomeException) r)
type ServerEffects = Eff '[Exc SomeException, Server]


-- Tidy up the following Server definition
-- We need the ability to handle exceptions in the interpreted code at least for proper handling
-- of database transactions:
type EServer a =  Server (Either SomeException a)

data Server v where
  Atomically     :: STM a -> EServer a
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  GetState       :: EServer OnlineState
  LogMessage     :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> EServer ()
  RunDb          :: Eff '[Exc SomeException, Database SqlBackend]  a -> EServer a
  SendEmail      :: !Mail -> EServer ()
  Timeout        :: ServerConstraint r => !Int -> Eff r a -> EServer a
  Notify         :: forall endpoint. (IsElem endpoint GonimoAPI, HasLink endpoint
                      , IsValidEndpoint endpoint, IsSubscribable endpoint GonimoAPI)
                      => Event -> Proxy endpoint -> (MkLink endpoint -> URI) -> EServer ()


-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = either throwError return =<< send op
