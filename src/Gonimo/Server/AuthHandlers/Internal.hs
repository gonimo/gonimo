module Gonimo.Server.AuthHandlers.Internal where

import           Control.Monad.Freer             (Eff)
import           Data.Proxy                      (Proxy (Proxy))
import           Gonimo.Database.Effects.Servant (get404)
import           Gonimo.Server.DbEntities        (Family, FamilyId, AccountId)
import           Gonimo.WebAPI                   (ListDevicesR, GetFamilyDevicesR, GetFamilyR)
import           Servant.API                     ((:>), Capture, Get, JSON)
import           Gonimo.Server.Effects (runDb, ServerConstraint)


listDevicesEndpoint  :: Proxy ("onlineStatus" :> ListDevicesR)
listDevicesEndpoint = Proxy

listFamiliesEndpoint :: Proxy ("accounts" :> Capture "accountId" AccountId :> "families" :> Get '[JSON] [FamilyId])
listFamiliesEndpoint = Proxy

getDeviceInfosEndpoint :: Proxy ("families" :> Capture "familyId" FamilyId :> GetFamilyDevicesR)
getDeviceInfosEndpoint = Proxy

getFamilyEndpoint :: Proxy ("families" :> Capture "familyId" FamilyId :> GetFamilyR)
getFamilyEndpoint = Proxy

-- The following stuff should go somewhere else someday (e.g. to paradise):

-- | Get the family of the requesting device.
--
--   error 404 if not found.
--   TODO: Get this from in memory data structure when available.
getFamily :: ServerConstraint r => FamilyId -> Eff r Family
getFamily fid = runDb $ get404 fid
