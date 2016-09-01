{-# LANGUAGE GADTs #-}
module Gonimo.Database.Effects.Servant where
-- Little helpers integrating db functions with servant:

import           Control.Exception             (SomeException)
import           Control.Monad                 ((<=<))
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception (Exc (..))
import           Database.Persist              (Entity, Key, Unique)
import           Gonimo.Database.Effects
import           Gonimo.Server.Error

get404 :: FullDbConstraint backend a r => Key a -> Eff r a
get404 = serverErrOnNothing NotFound <=< get


getBy404 :: FullDbConstraint backend a r => Unique a -> Eff r (Entity a)
getBy404 = serverErrOnNothing NotFound <=< getBy

serverErrOnNothing :: (Member (Exc SomeException) r) => ServerError -> Maybe a -> Eff r a
serverErrOnNothing err Nothing = throwServer err
serverErrOnNothing _ (Just v) = return v
