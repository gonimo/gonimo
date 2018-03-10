{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Family entity
--   This module is intended to be imported qualified as FamilyAccount
module Gonimo.Server.Db.Account ( insert, Gonimo.Server.Db.Account.get
                                , joinFamily, getFamilyIds, getUser, getDevices
                                ) where

import           Control.Monad.Base               (MonadBase)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.State.Class        as State
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.Foldable                    (traverse_)
import           Data.Maybe
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Vector                      as V
import           Database.Persist                 (Entity(..), entityVal, (==.))
import qualified Database.Persist.Class           as Db
import           Database.Persist.Sql             (SqlBackend)

import           Gonimo.Database.Effects.Servant
import qualified Gonimo.Database.Effects.Servant  as Db
import qualified Gonimo.Db.Entities               as Db
import           Gonimo.Server.Db.IsDb
import           Gonimo.Server.Error              (ServerError (..))
import           Gonimo.Server.NameGenerator      (Predicates)
import qualified Gonimo.Server.NameGenerator      as Gen
import qualified Gonimo.SocketAPI.Types           as API



-- | Create a new account in the database.
insert :: MonadIO m => API.Account -> ReaderT SqlBackend m API.AccountId
insert = fmap fromDb . Db.insert . toDb

get :: (MonadBase IO m, MonadIO m) => API.AccountId -> ReaderT SqlBackend m API.Account
get aid' = fmap fromDb . getErr (NoSuchAccount aid') . toDb $ aid'

getFamilyIds :: MonadIO m => API.AccountId -> ReaderT SqlBackend m [API.FamilyId]
getFamilyIds aid = do
  entities <- Db.selectList [ Db.FamilyAccountAccountId ==. toDb aid ] []
  pure $ map (fromDb . Db.familyAccountFamilyId . entityVal) entities

getDevices :: MonadIO m => API.AccountId -> ReaderT SqlBackend m [(API.DeviceId, API.Device)]
getDevices aid = do
  entities <- Db.selectList [ Db.DeviceAccountId ==. toDb aid ] []
  let fromEntity (Entity k v) = (fromDb k, fromDb v)
  pure $ map fromEntity entities

getUser :: MonadIO m => API.AccountId -> ReaderT SqlBackend m (Maybe (API.UserId, API.User))
getUser aid' = runMaybeT $ do
  let aid = toDb aid'
  Entity userId' user' <- MaybeT $ Db.getBy $ Db.AccountIdUser aid
  pure (fromDb userId', fromDb user')


-- | Make an account join a family
--
--   Also takes care of setting associated device names.
joinFamily :: Predicates -> API.FamilyAccount -> ReaderT SqlBackend IO ()
joinFamily predPool = joinFamily' predPool . toDb

-- | Make an account join a family
--
--   Also takes care of setting associated device names.
joinFamily' :: Predicates -> Db.FamilyAccount -> ReaderT SqlBackend IO ()
joinFamily' predPool familyAccount' = do
  Db.insert_  familyAccount'

  let getAccountDevices accountId' = Db.selectList [ Db.DeviceAccountId ==. accountId' ] []

  devices <- getAccountDevices (Db.familyAccountAccountId familyAccount')
  let brokenDevices = filter (isNothing . Db.deviceName . entityVal) devices

  familyAccountIds <- map (Db.familyAccountAccountId . entityVal)
                      <$> Db.selectList [ Db.FamilyAccountFamilyId ==. Db.familyAccountFamilyId familyAccount' ] []
  familyDevices <- concat <$> traverse getAccountDevices familyAccountIds
  let blackList = Set.fromList $ mapMaybe (Db.deviceName . entityVal) familyDevices
  family <- Db.get404 (Db.familyAccountFamilyId familyAccount')
  let initList = fmap (flip Gen.makeDeviceName $ Db.familyName family) predPool
  let filtered = V.filter (not . flip Set.member blackList) initList
  fixedDevices <- flip evalStateT filtered $ traverse fixDeviceName brokenDevices
  let myReplace (Entity k v) = Db.replace k v
  traverse_ myReplace fixedDevices


fixDeviceName :: (MonadState (V.Vector Text) m, MonadIO m)
                 => Entity Db.Device -> m (Entity Db.Device)
fixDeviceName (Entity k v) = do
  name <- genDeviceName
  pure $ Entity k (v { Db.deviceName = Just name })



genDeviceName :: (MonadState (V.Vector Text) m, MonadIO m) => m Text
genDeviceName = do
  nameList <- State.get
  if V.null nameList
    then pure "Mr. X"
    else do
    name <- Gen.getRandomVectorElement nameList
    put $ V.filter (/= name) nameList
    pure name


