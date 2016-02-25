module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , AuthServerConstraint
  , ServerException(..)
  , ServerConstraint
  , sendServer
  ) where

import Control.Exception.Base (SomeException, Exception)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Control.Monad.Logger (Loc, LogLevel, LogSource, ToLogStr)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Text (Text)


import Data.Time.Clock (UTCTime)
import GHC.Generics

import Network.Mail.Mime (Mail)
import Gonimo.Database.Effects
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Types (Entity(..))
import Crypto.Random (GenError)

-- Tidy up the following Server definition
type EServer a =  Server (Either ServerException a)

data Server v where
  SendEmail :: !Mail -> EServer ()
  LogMessage :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> EServer ()
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  RunDb :: Eff '[Exc DbException, Database SqlBackend]  a -> EServer a

data ServerException =
    NotFound
    | BadRequest Text
    | RandomGeneratorException GenError
    | TeaPot
    | SystemException SomeException deriving (Show, Generic, Typeable)


-- Necessary for DB handling:
instance Exception ServerException

-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc ServerException) r)

-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v
