{-|
Module      : Control.Monad.RIO
Description : Reader + IO monad
Copyright   : (c) Robert Klotzner, 2018
To be used concrete as RIO env a with polymorphic env.
For details see: https://www.fpcomplete.com/blog/2017/07/the-rio-monad
-}
module Control.Monad.RIO ( -- * Types
                           RIO(..)
                         , runRIO
                         , HasLogFunc(..)
                         ) where



import           Control.Monad.Logger           (MonadLogger(..),
                                                 MonadLoggerIO(..), askLoggerIO,
                                                 Loc, LogSource, LogLevel, LogStr, toLogStr)
import           Control.Monad.Trans.Reader     (ReaderT(..))
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Lens (Getting, view)
import           Control.Monad.Catch  as X (MonadThrow (..))
import           Control.Monad.Reader           (MonadReader)
import           Control.Monad.IO.Unlift



-- | Reader IO Monad specialized on IO.
--
--   RIO as in: https://www.fpcomplete.com/blog/2017/07/the-rio-monad
newtype RIO env a = RIO { unRIO :: (ReaderT env IO a) }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow,MonadUnliftIO)



runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)


class HasLogFunc env where
  logFuncL :: Getting r env (Loc -> LogSource -> LogLevel -> LogStr -> IO ())


instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog a b c d = do
    f <- view logFuncL
    liftIO $ f a b c $ toLogStr d

instance HasLogFunc env => MonadLoggerIO (RIO env) where
  askLoggerIO = view logFuncL
