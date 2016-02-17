{-- Freer extensible effects for database access, based on persistent types.
--}
module Gonimo.Database.Effects where

import Control.Exception.Base (SomeException)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))


import Database.Persist (PersistEntity(..))
import GHC.Generics

-- Tidy up the following Database definition
type EDatabase a =  Database (Either DbException a)

                  
data Database v where
  Insert :: PersistEntity a => a -> EDatabase (Key a)
  Insert_ :: PersistEntity a => a -> EDatabase ()
  Get :: PersistEntity a => Key a -> EDatabase a

data DbException =
  NotFoundException
  | SystemException SomeException deriving (Show, Generic)

insert :: (DbConstraint r, PersistEntity a) => a -> Eff r (Key a)
insert = sendDb . Insert

insert_ :: (DbConstraint r, PersistEntity a) => a -> Eff r ()
insert_ = sendDb . Insert_

get :: (DbConstraint r, PersistEntity a) => Key a -> Eff r a
get = sendDb . Get

-- Type synonym for constraints on Database API functions, requires ConstraintKinds language extension:
type DbConstraint r = (Member Database r, Member (Exc DbException) r)
type DbEffects = Eff '[Exc DbException, Database]

-- Send a server operation, that is an operation that might fail:
sendDb :: DbConstraint r => EDatabase a -> Eff r a
sendDb op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v
