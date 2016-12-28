{-# LANGUAGE GADTs #-}
module Gonimo.Database.Effects.PersistDatabase where


import Control.Exception.Lifted (try)
import Control.Monad.Freer.Internal (Eff(..), Arrs, decomp, qApp)
import Control.Monad.Freer.Exception (Exc(..), runError)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Monoid ((<>))
import Database.Persist
import Gonimo.Database.Effects (Database(..))
import Control.Exception.Base (SomeException)

type PersistConstraint backend = (HasPersistBackend backend backend, PersistStore backend, PersistUnique backend, PersistQuery backend)

runExceptionDatabase :: forall w backend .PersistConstraint backend
                        => Eff (Exc SomeException ': '[Database backend]) w
                        -> ReaderT backend IO (Either SomeException w)
runExceptionDatabase = runDatabase . runError


runDatabase :: forall w backend . PersistConstraint backend
               => Eff '[Database backend] (Either SomeException w)
               -> ReaderT backend IO (Either SomeException w)
runDatabase (Val v)  = return v
runDatabase (E u' q) = case decomp u' of
  Right (Insert e)    -> execIO q $ insert e
  Right (Insert_ e)   -> execIO q $ insert_ e
  Right (Replace k a) -> execIO q $ replace k a
  Right (Delete k)    -> execIO q $ delete k
  Right (DeleteBy k)  -> execIO q $ deleteBy k
  Right (Get k)       -> execIO q $ get k
  Right (GetBy k)     -> execIO q $ getBy k
  Right (SelectList fs ss) -> execIO q $ selectList fs ss
  Left _ -> error impossibleMessage

execIO :: PersistConstraint backend
          => Arrs '[Database backend] (Either SomeException b) (Either SomeException w)
          -> ReaderT backend IO b
          -> ReaderT backend IO (Either SomeException w)
execIO q action = try action >>= runDatabase . qApp q

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
