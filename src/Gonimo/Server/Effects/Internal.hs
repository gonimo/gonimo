module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerException(..)
  , ServerConstraint
  , sendServer
  ) where

import Control.Exception.Base (SomeException)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import Gonimo.Database.Effects (DbEffects)

import Network.Mail.Mime (Mail)


import Data.Time.Clock (UTCTime)

-- Tidy up the following Server definition
type EServer a =  Server (Either ServerException a)

data Server v where
  SendEmail :: !Mail -> EServer ()
  LogMessage :: !Text -> EServer ()
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  RunDb :: DbEffects a -> EServer a

data ServerException =
    NotFoundException Text
  | SystemException SomeException deriving (Show, Generic)


-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc ServerException) r)


-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v

