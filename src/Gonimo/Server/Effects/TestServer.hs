module Gonimo.Server.Effects.TestServer where

import Gonimo.Server.Effects.API.Internal
import Control.Exception.Base (try)
import Network.Mail.SMTP (sendMail)
import Data.Bifunctor (first)

import Control.Monad.Freer.Exception (runError)
import Control.Monad.Freer.Internal (Eff(..))
import Control.Monad.Freer.Exception (Exc(..))
import Control.Monad.Freer.Internal (decomp)
import Control.Monad.Freer.Internal (tsingleton)
import Control.Monad.Freer.Internal (qApp)
import Data.FTCQueue ((><))
import Control.Monad.Freer.Internal (Union)
import Data.Monoid ((<>))


runErrorServer :: Eff (Exc ServerError ': '[Server]) w  -> IO (Either ServerError w)
runErrorServer = runServer . runError

runServer :: Eff '[Server] (Either ServerError w) -> IO (Either ServerError w)
runServer (Val v) = return v
runServer (E u' q) = case decomp u' of
  Right (SendEmail mail) -> serverTry (sendMail "localhost" mail)
                            >>= runServer . qApp q
  Left _ -> error $ "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
            <> "Really this can not be.\n\nAre you really sure you are seeing this?"
  
serverTry :: IO a -> IO (Either ServerError a)
serverTry op = first SystemException <$> try op
