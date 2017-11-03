{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with User entity
--   This module is intended to be imported qualified as User
module Gonimo.Server.Db.User where

import           Control.Monad.State.Class
import           Data.Maybe
import           Data.Text                        (Text)


import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.Foldable                    (traverse_)
import qualified Data.Set                         as Set
import qualified Data.Vector                      as V
import           Database.Persist                 (Entity, entityVal, (==.))
import           Database.Persist                 (Entity (..))
import qualified Database.Persist.Class           as Db
import           Database.Persist.Sql             (SqlBackend)

import qualified Gonimo.Database.Effects.Servant  as Db
import qualified Gonimo.Db.Entities               as Db
import qualified Gonimo.Server.NameGenerator      as Gen
import qualified Gonimo.SocketAPI.Types           as API
import           Gonimo.Types                     (Predicates)
import Gonimo.Server.Db.IsDb



-- | Create a new user in the database.
insert :: MonadIO m => API.User -> ReaderT SqlBackend m API.UserId
insert = fmap fromDb . Db.insert . toDb

