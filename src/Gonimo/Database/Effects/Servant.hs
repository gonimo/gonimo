{-# LANGUAGE GADTs #-}
module Gonimo.Database.Effects.Servant where
-- Little helpers integrating db functions with servant:

import           Control.Exception             (SomeException)
import           Control.Monad                 ((<=<))
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception (Exc (..))
import           Database.Persist              (Entity, Key, Unique, get, getBy)
import           Gonimo.Server.Error
import           Control.Monad.Error.Class      (throwError, MonadError)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Database.Persist.Sql           (SqlBackend)

get404 ::  Key a -> ReaderT SqlBackend IO a
get404 = getErr NotFound

getErr ::  ServerError -> Key a -> ReaderT SqlBackend IO a
getErr err = serverErrOnNothing err <=< get

getBy404 ::  Unique a -> ReaderT SqlBackend IO (Entity a)
getBy404 = getByErr NotFound

getByErr ::  ServerError -> Unique a -> ReaderT SqlBackend IO (Entity a)
getByErr err = serverErrOnNothing err <=< getBy

serverErrOnNothing :: ServerError -> Maybe a -> ReaderT SqlBackend IO a
serverErrOnNothing err Nothing = throwServer err
serverErrOnNothing _ (Just v) = return v
