{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects (
  Server
  , sendEmail
  , logMessage
  , genRandomBytes
  , getCurrentTime
  , runDb
  , ServerConstraint
  , ServerException(..)
  ) where



import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Gonimo.Server.Effects.Internal
import Network.Mail.Mime (Mail)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Gonimo.Database.Effects

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


logMessage :: ServerConstraint r => Text -> Eff r ()
logMessage = sendServer . LogMessage


genRandomBytes :: ServerConstraint r => Int -> Eff r ByteString
genRandomBytes = sendServer . GenRandomBytes

getCurrentTime :: ServerConstraint r => Eff r UTCTime
getCurrentTime = sendServer GetCurrentTime

runDb :: ServerConstraint r => DbEffects a -> Eff r a
runDb = sendServer . RunDb
