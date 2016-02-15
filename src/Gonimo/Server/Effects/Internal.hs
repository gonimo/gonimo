module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerError(..)
  , ServerConstraint
  , sendServer
  ) where

import Network.Mail.Mime (Mail)
import Control.Exception.Base (SomeException)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Control.Monad.Freer (send, Member, Eff)
import Data.Text (Text)
import GHC.Generics
import TextShow
import TextShow.Generic
import Gonimo.Types

-- Tidy up the following Server definition
type EServer a =  Server (Either ServerError a)

data Server v where 
  SendEmail :: !Mail -> EServer ()
  LogMessage :: !Text -> EServer ()
  -- db stuff:
  insertAccount :: !Account -> EServer AccountId

data ServerError =
  SystemException SomeException deriving (Show, Generic)


-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc ServerError) r)


-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v

