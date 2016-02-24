module Gonimo.Database.Effects.PersistDatabase where


import Control.Exception.Lifted (try)
import Control.Monad.Freer.Internal (Eff(..), Arrs, decomp, qApp)
import Control.Monad.Freer.Exception (Exc(..), runError)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Database.Persist
import Gonimo.Database.Effects (Database(..), DbException(..))




runExceptionDatabase :: forall w backend . (HasPersistBackend backend backend, PersistStore backend, PersistUnique backend)
                        => Eff (Exc DbException ': '[Database backend]) w
                        -> ReaderT backend IO (Either DbException w)
runExceptionDatabase = runDatabase . runError


runDatabase :: forall w backend . (HasPersistBackend backend backend, PersistStore backend, PersistUnique backend)
               => Eff '[Database backend] (Either DbException w)
               -> ReaderT backend IO (Either DbException w)
runDatabase (Val v)  = return v
runDatabase (E u' q) = case decomp u' of
  Right (Insert e)    -> execIO q $ insert e
  Right (Insert_ e)   -> execIO q $ insert_ e
  Right (Replace k a) -> execIO q $ replace k a
  Right (Delete k)    -> execIO q $ delete k
  Right (Get k)       -> dbMayTry (get k) >>= runDatabase . qApp q
  Right (GetBy k)     -> dbMayTry (getBy k) >>= runDatabase . qApp q
  Left _ -> error impossibleMessage

execIO :: (HasPersistBackend backend backend, PersistStore backend, PersistUnique backend)
          => Arrs '[Database backend] (Either DbException b) (Either DbException w)
          -> ReaderT backend IO b
          -> ReaderT backend IO (Either DbException w)
execIO q action = dbTry action >>= runDatabase . qApp q

dbTry :: (HasPersistBackend backend backend, PersistStore backend)
         => ReaderT backend IO a -> ReaderT backend IO (Either DbException a)
dbTry op = first SystemException <$> try (liftPersist op)

dbMayTry :: (HasPersistBackend backend backend, PersistStore backend)
         => ReaderT backend IO (Maybe a) -> ReaderT backend IO (Either DbException a)
dbMayTry op = do
  r <- try $ liftPersist op
  case r of
    Left e -> return $ Left (SystemException e)
    Right Nothing -> return $ Left NotFoundException
    Right (Just v) -> return $ Right v


impossibleMessage :: String
impossibleMessage =
  "This cannot happen. Really! If you see this message, then, well - the impossible happened!\n"
  <> "Really this can not be.\n\nAre you really sure you are seeing this?"


{--
-- Little experiment of composing multiple IO interpreters

data Writer a where
  Write :: Text -> Writer ()

data Reader a where
  Read :: Reader Text

runWriter :: Eff (Writer ': r) a -> IO (Eff r a)
runWriter (Val v) = return (return  v)
runWriter (E u' q) = case decomp u' of
  Right (Write t) -> T.putStrLn t >>= runWriter . qApp q
  Left  u -> do
    k <- qCompIO q runWriter
    return $ E u (tsingleton k)


type IOArr r a b = a -> IO (Eff r b)

qCompIO :: Arrs r a b -> (Eff r b -> IO (Eff r' c)) -> IOArr r' a c
qCompIO g h a = h $ qApp g a


--}
