module Gonimo.Server.Effects.TestServer (
  runErrorServer
  , ServerEffects) where

import Gonimo.Server.Effects.Internal
import Control.Exception.Base (try)
import Network.Mail.SMTP (sendMail)
import Data.Bifunctor (first)

import Control.Monad.Freer.Exception (runError)
import Control.Monad.Freer.Internal (Eff(..), Arrs)
import Control.Monad.Freer.Exception (Exc(..))
import Control.Monad.Freer.Internal (decomp)
import Control.Monad.Freer.Internal (tsingleton)
import Control.Monad.Freer.Internal (qApp)
import Data.FTCQueue ((><))
import Control.Monad.Freer.Internal (Union)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T


type ServerEffects = Eff '[Exc ServerError, Server]

runErrorServer :: Eff (Exc ServerError ': '[Server]) w  -> IO (Either ServerError w)
runErrorServer = runServer . runError

runServer :: forall w . Eff '[Server] (Either ServerError w) -> IO (Either ServerError w)
runServer (Val v) = return v
runServer (E u' q) = case decomp u' of
  Right (SendEmail mail)         -> execIO q $ sendMail "localhost" mail
  Right (LogMessage message)     -> execIO q $ T.putStrLn message 
  Left  _                        -> error impossibleMessage
  
execIO :: Arrs '[Server] (Either ServerError b) (Either ServerError w)
          -> IO b
          -> IO (Either ServerError w)
execIO q action = serverTry action >>= runServer . qApp q
  
serverTry :: IO a -> IO (Either ServerError a)
serverTry op = first SystemException <$> try op


impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"
