{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects.API (
  Server
  , sendEmail
  , ServerConstraint
  , ServerError
  ) where

import Network.Mail.Mime (Mail)
import Control.Monad.Freer (Eff, send, Member)
import Control.Monad.Freer.Exception (Exc)
import Control.Exception.Base (SomeException)
import Gonimo.Server.Effects.API.Internal
import Data.Text (Text)


sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


debugPrint :: ServerConstraint r => Text -> Eff r ()
debugPrint = sendServer . DebugPrint
