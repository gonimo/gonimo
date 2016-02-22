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
import Control.Monad.Freer.Exception (Exc)
import Control.Monad.Logger (Loc, LogLevel, LogSource, ToLogStr)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Database.Persist.Class (PersistStore)
import Gonimo.Database.Effects
import Gonimo.Server.Effects.Internal
import Network.Mail.Mime (Mail)

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


logMessage :: (ServerConstraint r, ToLogStr msg)  => Loc -> LogSource -> LogLevel -> msg -> Eff r ()
logMessage loc ls ll msg = sendServer $ LogMessage loc ls ll msg


genRandomBytes :: ServerConstraint r => Int -> Eff r ByteString
genRandomBytes = sendServer . GenRandomBytes

getCurrentTime :: ServerConstraint r => Eff r UTCTime
getCurrentTime = sendServer GetCurrentTime

runDb :: (ServerConstraint r, PersistStore backend) => Eff '[Exc DbException, Database backend]  a -> Eff r a
runDb = sendServer . RunDb

{--
-- Orphan instance - extensible effects and typeclasses don't really play well together ...
instance MonadLogger (Eff '[Exc ServerException, Server])  where
  monadLoggerLog = logMessage
--}
