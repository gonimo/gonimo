-- General utility functions that did not fit anywhere else (or are PRs for other projects):
-- or should really exist somewhere in a suitable library but don't.
module Gonimo.Util where



import           Control.Exception             (Exception, SomeException,
                                                toException)


import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Data.Typeable                 (Typeable)
import           Servant.Server



throwException :: (Member (Exc SomeException) r, Exception e) => e -> Eff r a
throwException = throwError . toException


throwServant :: Member (Exc SomeException) r => ServantErr -> Eff r a
throwServant = throwException . ServantException

newtype ServantException = ServantException {
  unwrapServantErr :: ServantErr
  } deriving (Show, Typeable, Eq)

instance Exception ServantException
