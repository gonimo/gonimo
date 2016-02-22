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

import Control.Monad.Freer.Internal (qApp)


import Data.Monoid ((<>))
import qualified Data.Text.IO as T


type ServerEffects = Eff '[Exc ServerException, Server]

runErrorServer :: Eff (Exc ServerException ': '[Server]) w  -> IO (Either ServerException w)
runErrorServer = runServer . runError

runServer :: forall w . Eff '[Server] (Either ServerException w) -> IO (Either ServerException w)
runServer (Val v) = return v
runServer (E u' q) = case decomp u' of
  Right (SendEmail mail)         -> execIO q $ sendMail "localhost" mail
  Right (LogMessage message)     -> execIO q $ T.putStrLn message
  Right (RunDb db)               -> execIO q $ 
  Left  _                        -> error impossibleMessage

execIO :: Arrs '[Server] (Either ServerException b) (Either ServerException w)
          -> IO b
          -> IO (Either ServerException w)
execIO q action = serverTry action >>= runServer . qApp q

serverTry :: IO a -> IO (Either ServerException a)
serverTry op = first SystemException <$> try op


impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"
