module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerError(..)
  , ServerConstraint
  , sendServer
  ) where

import Control.Exception.Base (SomeException)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist
import GHC.Generics
import Gonimo.Types
import Network.Mail.Mime (Mail)
import TextShow
import TextShow.Generic
import Data.Time.Clock (UTCTime)

-- Tidy up the following Server definition
type EServer a =  Server (Either ServerError a)

data Server v where 
  SendEmail :: !Mail -> EServer ()
  LogMessage :: !Text -> EServer ()
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  -- db stuff:
  InsertDb :: PersistEntity a => a -> EServer (Key a)
  InsertDb_ :: PersistEntity a => a -> EServer ()
  GetDb :: PersistEntity a => Key a -> EServer a

data ServerError =
    NotFoundException
  | SystemException SomeException deriving (Show, Generic)


-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc ServerError) r)


-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v

