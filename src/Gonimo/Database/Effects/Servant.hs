module Gonimo.Database.Effects.Servant where
-- Little helpers integrating db functions with servant:

import           Control.Exception             (SomeException)
import           Control.Monad                 ((<=<))
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception (Exc (..))
import           Database.Persist              (Entity, Key, Unique)
import           Gonimo.Database.Effects
import           Gonimo.Util
import           Servant.Server

get404 :: DbConstraint backend a r => Key a -> Eff r a
get404 = servantErrOnNothing err404 <=< get


getBy404 :: DbConstraint backend a r => Unique a -> Eff r (Entity a)
getBy404 = servantErrOnNothing err404 <=< getBy

servantErrOnNothing :: (Member (Exc SomeException) r) => ServantErr -> Maybe a -> Eff r a
servantErrOnNothing err Nothing = throwServant err
servantErrOnNothing _ (Just v) = return v
