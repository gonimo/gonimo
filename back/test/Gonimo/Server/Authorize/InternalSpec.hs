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
import qualified Gonimo.Server.Cache.Internal            as Cache
import           Gonimo.Server.Cache.Internal         (Model(..))
import           Gonimo.Server.Clients.ClientStatus (ClientStatuses, ClientStatus(..))
import qualified Gonimo.Server.Clients.ClientStatus as ClientStatus


import           Gonimo.Server.Error
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Model
import           Gonimo.Server.Authorize.Internal
import qualified Gonimo.Server.Cache.Devices        as Devices
import qualified Gonimo.Server.Cache.FamilyAccounts as FamilyAccounts
import qualified Gonimo.Server.Cache.IndexedTable   as Table
import qualified Gonimo.Server.Cache.Invitations    as Invitations


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
  describe "ClaimInvitation" $ do
    it "succeeds always" $ do
      deny (mkRequest 1 (ClaimInvitation (Secret "haha"))) `shouldBe` Nothing
  describe "AnswerInvitation" $ do
    it "fails if invitation not yet claimed" $ do
      deny (mkRequest 1 (AnswerInvitation (InvitationId 1) InvitationAccept)) `shouldBe` Just Forbidden
    it "fails if claimed by another account." $ do
      deny (mkRequest 1 (AnswerInvitation (InvitationId 2) InvitationAccept)) `shouldBe` Just Forbidden
    it "succeeds if device owns the invitation" $ do
      deny (mkRequest 9 (AnswerInvitation (InvitationId 3) InvitationAccept)) `shouldBe` Nothing
  describe "SendMessage" $ do
    it "fails if both devices are not within the same family" $ do
      deny (mkRequest 9 (SendMessage (DeviceId 1) "Haha")) `shouldBe` Just Forbidden
    it "fails if one of the devices is not online in the right family" $ do
      deny (mkRequest 9 (SendMessage (DeviceId 8) "Haha")) `shouldBe` Just Forbidden
    it "fails if one device does not exist at all" $ do
      deny (mkRequest 9 (SendMessage (DeviceId 18) "Haha")) `shouldBe` Just Forbidden
    it "succeeds if both devices are online within the same family" $ do
      deny (mkRequest 9 (SendMessage (DeviceId 10) "Haha")) `shouldBe` Nothing
  describe "UpdateServer" $ updateServerSpec

updateServerSpec :: Spec
updateServerSpec = do
  describe "OnChangedFamilyname" $ do
    it "fails if device is not online in that family" $ do
      deny (mkUpdate 8 (OnChangedFamilyName (FamilyId 8) "New Name")) `shouldBe` Just Forbidden
    it "fails if device is not a member of that family" $ do
      deny (mkUpdate 1 (OnChangedFamilyName (FamilyId 9) "New Name")) `shouldBe` Just Forbidden
    it "succeeds if device is online in that family" $ do
      deny (mkUpdate 9 (OnChangedFamilyName (FamilyId 9) "New Name")) `shouldBe` Nothing
  describe "OnRemovedFamilyMember" $ do
    it "fails for non family members" $ do
      deny (mkUpdate 1 (OnRemovedFamilyMember (FamilyId 9) (AccountId 9))) `shouldBe` Just Forbidden
    it "fails for family members that are not online" $ do
      deny (mkUpdate 8 (OnRemovedFamilyMember (FamilyId 9) (AccountId 9))) `shouldBe` Just Forbidden
    it "succeeds for family members (that are online)" $ do
      deny (mkUpdate 9 (OnRemovedFamilyMember (FamilyId 9) (AccountId 10))) `shouldBe` Nothing
  describe "OnRemovedFamilyInvitation" $ do
    it "fails non family members" $ do
      deny (mkUpdate 1 (OnRemovedFamilyInvitation (FamilyId 9) (InvitationId 1))) `shouldBe` Just Forbidden
    it "fails non online family members" $ do
      deny (mkUpdate 8 (OnRemovedFamilyInvitation (FamilyId 9) (InvitationId 1))) `shouldBe` Just Forbidden
    it "fails for invitations not belonging to the given family" $ do
      deny (mkUpdate 9 (OnRemovedFamilyInvitation (FamilyId 9) (InvitationId 4))) `shouldBe` Just Forbidden
    it "succeeds for online family members" $ do
      deny (mkUpdate 9 (OnRemovedFamilyInvitation (FamilyId 9) (InvitationId 1))) `shouldBe` Nothing
  describe "OnChangedDeviceName" $ do
    it "fails for non online family members" $ do
      deny (mkUpdate 8 (OnChangedDeviceName (DeviceId 9) "Heinzi")) `shouldBe` Just Forbidden
    it "works for online family members" $ do
      deny (mkUpdate 9 (OnChangedDeviceName (DeviceId 8) "Heinzi")) `shouldBe` Just Forbidden
  describe "OnChangedDeviceStatus" $ do
    it "fails for all other devices" $ do
      deny (mkUpdate 8 (OnChangedDeviceStatus (DeviceId 9) (FamilyId 9) Online)) `shouldBe` Just Forbidden
    it "works for the device itself" $ do
      deny (mkUpdate 9 (OnChangedDeviceStatus (DeviceId 9) (FamilyId 9) Online)) `shouldBe` Nothing
  describe "OnChangedInvitationDelivery" $ do
    it "fails for non online family devices" $ do
      deny (mkUpdate 8 (OnChangedInvitationDelivery (InvitationId 1) OtherDelivery)) `shouldBe` Just Forbidden
    it "fails for invitations not belonging to the current family" $ do
      deny (mkUpdate 9 (OnChangedInvitationDelivery (InvitationId 4) OtherDelivery)) `shouldBe` Just Forbidden
    it "works for online devices in the right family" $ do
      deny (mkUpdate 9 (OnChangedInvitationDelivery (InvitationId 1) OtherDelivery)) `shouldBe` Nothing
  where
    mkUpdate client upd = mkRequest client (UpdateServer upd)


mkRequest client msg = AuthRequest myCache myClients (DeviceId client) msg

myCache :: Cache.Model
myCache
  = Cache.Model
    { _families = Map.fromList [(FamilyId 9, Family (parseFamilyName "Family9") timeStamp timeStamp [])]
    , _invitations = Invitations.make
                     $ Map.fromList [ (InvitationId 1, Invitation (Secret "haha") (FamilyId 9) timeStamp (EmailInvitation "test@test.com")  (DeviceId 9) Nothing)
                                    , (InvitationId 2, Invitation (Secret "haha1") (FamilyId 9) timeStamp (EmailInvitation "test@test.com")  (DeviceId 9) (Just (AccountId 2)))
                                    , (InvitationId 3, Invitation (Secret "haha2") (FamilyId 9) timeStamp (EmailInvitation "test@test.com")  (DeviceId 9) (Just (AccountId 9)))
                                    , (InvitationId 4, Invitation (Secret "haha3") (FamilyId 1) timeStamp (EmailInvitation "test@test.com")  (DeviceId 9) (Just (AccountId 9)))
                                    ]
    , _accounts = Map.fromList [ (AccountId 9, Account timeStamp)
                               , (AccountId 1, Account timeStamp)
                               , (AccountId 2, Account timeStamp)
                               , (AccountId 10, Account timeStamp)
                               ]
    , _familyAccounts = FamilyAccounts.make
                        $ Map.fromList [ (FamilyAccountId 9, FamilyAccount (AccountId 9) (FamilyId 9) timeStamp Nothing)
                                       , (FamilyAccountId 10, FamilyAccount (AccountId 10) (FamilyId 9) timeStamp Nothing)
                                       ]
    , _devices = Devices.make
                 $ Map.fromList [ (DeviceId 9, Device (Just "inFamilyDevice") (ourAuthToken "device 9") (AccountId 9) timeStamp "Some very weird browser.")
                                , (DeviceId 8, Device (Just "inFamilyDevice2") (ourAuthToken "device 8") (AccountId 9) timeStamp "Some very weird browser.")
                                , (DeviceId 1, Device (Just "notInFamilyDevice") (ourAuthToken "device 1") (AccountId 1) timeStamp "Some very weird browser.")
                                , (DeviceId 10, Device (Just "inFamilyDevice3") (ourAuthToken "device 10") (AccountId 9) timeStamp "Some very weird browser.")
                                , (DeviceId 11, Device (Just "inFamilyDevice4") (ourAuthToken "device 11") (AccountId 10) timeStamp "Some very weird browser.")
                                ]
    }

myClients :: ClientStatuses
myClients
  = ClientStatus.makeStatuses
    $ Map.fromList [ (DeviceId 9, ClientStatus Online (Just (FamilyId 9)))
                   , (DeviceId 10, ClientStatus Online (Just (FamilyId 9)))
                   ]

ourAuthToken :: ByteString -> AuthToken
ourAuthToken bs = GonimoSecret (Secret bs)

timeStamp :: UTCTime
timeStamp = UTCTime { utctDay = ModifiedJulianDay 0
                    , utctDayTime = secondsToDiffTime 9
                    }
