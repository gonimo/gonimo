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
  , insertDb
  , insertDb_
  , ServerConstraint
  , ServerError(..)
  ) where


import Control.Exception.Base (SomeException)
import Control.Monad.Freer (Eff, send, Member)
import Control.Monad.Freer.Exception (Exc)
import Data.Text (Text)
import Database.Persist.Class (Key, PersistEntity)
import Gonimo.Server.Effects.Internal
import Network.Mail.Mime (Mail)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


logMessage :: ServerConstraint r => Text -> Eff r ()
logMessage = sendServer . LogMessage


genRandomBytes :: ServerConstraint r => Int -> Eff r ByteString
genRandomBytes = sendServer . GenRandomBytes

getCurrentTime :: ServerConstraint r => Eff r UTCTime
getCurrentTime = sendServer GetCurrentTime

insertDb :: (ServerConstraint r, PersistEntity a) => a -> Eff r (Key a)
insertDb = sendServer . InsertDb
             
insertDb_ :: (ServerConstraint r, PersistEntity a) => a -> Eff r ()
insertDb_ = sendServer . InsertDb_

getDb :: (ServerConstraint r, PersistEntity a) => Key a -> Eff r a
getDb = sendServer . GetDb
