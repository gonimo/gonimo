-- Little helpers integrating db functions with servant:

module Gonimo.Database.Effects.Servant where



import Database.Persist (Key, Entity, Unique)
import Gonimo.Database.Effects
import Control.Monad.Freer.Exception
import Control.Monad.Freer
import Servant.Server
import Control.Monad ((<=<))
import Control.Exception (SomeException)
import Gonimo.Util



get404 :: DbConstraint backend a r => Key a -> Eff r a
get404 = servantErrOnNothing err404 <=< get


getBy404 :: DbConstraint backend a r => Unique a -> Eff r (Entity a)
getBy404 = servantErrOnNothing err404 <=< getBy

servantErrOnNothing :: (Member (Exc SomeException) r) => ServantErr -> Maybe a -> Eff r a
servantErrOnNothing err Nothing = throwServant err
servantErrOnNothing _ (Just v) = return v
