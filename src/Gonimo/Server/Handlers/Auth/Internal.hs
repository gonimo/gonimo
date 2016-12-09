module Gonimo.Server.Handlers.Auth.Internal where

import           Data.Proxy                      (Proxy (Proxy))
import           Gonimo.Database.Effects.Servant (get404)
import           Gonimo.Server.Db.Entities       (AccountId, Family, FamilyId)
import           Gonimo.Server.Effects           (MonadServer, runDb)
import           Gonimo.WebAPI                   (GetFamilyDevicesR, GetFamilyR,
                                                  ListDevicesR)
import           Servant.API                     ((:>), Capture, Get, JSON)


listDevicesEndpoint  :: Proxy ("session" :> ListDevicesR)
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
getFamily :: MonadServer m => FamilyId -> m Family
getFamily fid = runDb $ get404 fid
