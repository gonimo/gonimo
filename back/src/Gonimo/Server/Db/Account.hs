{-# LANGUAGE TypeFamilies #-}
-- | Functions for working with Family entity
--   This module is intended to be imported qualified as FamilyAccount
module Gonimo.Server.Db.Account (joinFamily) where

import           Control.Monad.State.Class
import           Data.Text                       (Text)
import           Data.Maybe


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
import qualified Gonimo.Db.Entities        as Db
import qualified Gonimo.Server.NameGenerator      as Gen
import           Gonimo.Types              (Predicates)

-- | Make an account join a family
--
--   Also takes care of setting associated device names.
joinFamily :: Predicates -> Db.FamilyAccount -> ReaderT SqlBackend IO ()
joinFamily predPool familyAccount' = do
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
  nameList <- get
  if V.null nameList
    then pure "Mr. X"
    else do
    name <- Gen.getRandomVectorElement nameList
    put $ V.filter (/= name) nameList
    pure name


