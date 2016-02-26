-- Little helpers for interfacing with servant:
module Gonimo.Server.Effects.Servant where

import Control.Monad.Freer.Exception
import Control.Monad.Freer
import Gonimo.Server.Effects


type ServantServerConstraint r = (Member (Exc ServantErr) r, ServerConstraint r)

runExceptionDb :: (ServantServerConstraint r) => Eff (Exc ServantErr ': '[Database SqlBackend])  a -> Eff r a
runExceptionDb = runDb . runError
