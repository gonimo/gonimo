{-# Language OverloadedStrings #-}
module Gonimo.Server.Authorize.InternalSpec where

import Test.Hspec


import           Control.Lens
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time






import           Gonimo.Prelude
import           Gonimo.Server.Cache.Internal            (HasSampled (..), Sampled(..))
import qualified Gonimo.Server.Cache.Internal            as Cache
import           Gonimo.Server.Clients.Internal (Clients (..), HasSampled (..), Sampled(..))
import qualified Gonimo.Server.Clients.Internal as Clients
import           Gonimo.Server.Error
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Model
import           Gonimo.Server.Authorize.Internal


spec :: Spec
spec = do
  describe "MakeFamily" $ do
    it "is always allowed" $ do
      deny (mkRequest 1 MakeFamily) == Nothing
        && deny (mkRequest 8 MakeFamily) == Nothing
  describe "MakeInvitation" $ do
    it "works if the device is online" $ do
      deny (mkRequest 9 (MakeInvitation (FamilyId 9))) `shouldBe` Nothing
    it "does not work for an offline device" $ do
      deny (mkRequest 8 (MakeInvitation (FamilyId 9))) `shouldBe` Just Forbidden
    it "does not work for a device which is not a family member" $ do
      deny (mkRequest 1 (MakeInvitation (FamilyId 9))) `shouldBe` Just Forbidden
  where
    mkRequest client msg = AuthRequest myCache myClients (DeviceId client) msg


myCache :: Cache.Sampled
myCache
  = Cache.Sampled
    { _sampledFamilies = Map.fromList [(FamilyId 9, Family (parseFamilyName "Family9") timeStamp timeStamp [])]
    , _sampledInvitations = Map.fromList [(InvitationId 1, Invitation (Secret "haha") (FamilyId 9) timeStamp (EmailInvitation "test@test.com")  (DeviceId 9) Nothing)
                                          ]
    , _sampledFamilyInvitations = Map.fromList [(FamilyId 9, [InvitationId 1])
                                                ]
    , _sampledAccountInvitations = Map.empty
    , _sampledAccounts = Map.fromList [ (AccountId 9, Account timeStamp)
                                      , (AccountId 1, Account timeStamp)
                                      ]
    , _sampledFamilyAccounts = Map.fromList [(FamilyId 9, [AccountId 9])]
    , _sampledAccountFamilies = Map.fromList [(AccountId 9, [FamilyId 9])]
    , _sampledFamilyAccountData = Map.empty -- Not correct, but should not do any harm.
    , _sampledDevices = Map.fromList [ (DeviceId 9, Device (Just "inFamilyDevice") (ourAuthToken "device 9") (AccountId 9) timeStamp "Some very weird browser.")
                                     , (DeviceId 8, Device (Just "inFamilyDevice2") (ourAuthToken "device 8") (AccountId 9) timeStamp "Some very weird browser.")
                                     , (DeviceId 1, Device (Just "notInFamilyDevice") (ourAuthToken "device 1") (AccountId 1) timeStamp "Some very weird browser.")
                                     ]
    }

myClients :: Clients.Sampled
myClients
  = Clients.Sampled
  { _sampledOnlineStatus = Map.fromList [(DeviceId 9, Online)]
  , _sampledSelectedFamily = Map.fromList [(DeviceId 9, FamilyId 9)]
  , _sampledBySelectedFamily = Map.fromList [(FamilyId 9, [DeviceId 9])]
  }

ourAuthToken :: ByteString -> AuthToken
ourAuthToken bs = GonimoSecret (Secret bs)

timeStamp :: UTCTime
timeStamp = UTCTime { utctDay = ModifiedJulianDay 0
                    , utctDayTime = secondsToDiffTime 9
                    }
