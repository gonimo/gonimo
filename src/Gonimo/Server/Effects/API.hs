{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects.API (
  Server
  , sendEmail
  ) where

import Network.Mail.Mime (Mail)
import Control.Monad.Freer (Eff, send, Member)
import Control.Monad.Freer.Exception (Exc)
import Control.Exception.Base (SomeException)

data Server v where 
  SendEmail :: !Mail -> Server ()

data ServerError =
  SystemException SomeException

-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member (Server) r,
                         Member (Exc ServerError) r)

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = send . SendEmail
