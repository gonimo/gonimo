{-# LANGUAGE TypeFamilies #-}
module Gonimo.Server.Db.Internal where

import           Control.Lens
import           Control.Monad.Freer             (Eff)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Control.Monad.Trans.State       (StateT (..))

import           Gonimo.Server.Error             (ServerError)
import           Database.Persist.Class          (PersistEntity, Key)

import           Control.Exception               (SomeException)
import           Control.Monad.Freer.Exception   (Exc)
import           Database.Persist.Sql            (SqlBackend, PersistEntityBackend)
import           Gonimo.Database.Effects         (Database)
import qualified Gonimo.Database.Effects         as Db
import           Gonimo.Database.Effects.Servant as Db

type UpdateT entity m a = StateT entity (MaybeT m) a

-- | Update db entity as specified by the given UpdateFamilyT - on Nothing, no update occurs.
updateRecord :: (PersistEntity record, SqlBackend ~ PersistEntityBackend record)
                => (Key record -> ServerError) -> Key record
                -> UpdateT record (Eff '[Exc SomeException, Database SqlBackend]) a
                -> MaybeT (Eff '[Exc SomeException, Database SqlBackend]) a
updateRecord noSuchRecord recordId f = do
  oldRecord <- lift $ Db.getErr (noSuchRecord recordId) recordId
  r <- flip runStateT oldRecord $ do
    r <- f
    newRecord <- get
    lift.lift $ Db.replace recordId newRecord
    pure r
  pure $ r^._1
