{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
import           Data.Text (Text)
import           GHC.Generics (Generic)
import Data.Aeson
import Control.Lens


-- data Resource = DeviceR | FamilyR | AccountR | InvitationR


data ReqCRUD r = Create (CreateData r)
               | Read (Identifier r) (ReadData r)
               | Update (Identifier r) (UpdateData r)
               | Delete (Identifier r) deriving (Generic)

instance ToJSON (ReqCRUD DeviceR)
instance FromJSON (ReqCRUD DeviceR)

data ResCRUD r = Created (Identifier r) (CreatedData r)
               | DidRead (Identifier r) (DidReadData r)
               | Updated (Identifier r) (UpdatedData r)
               | Deleted (Identifier r)
               deriving Generic

instance ToJSON (ResCRUD DeviceR)
instance FromJSON (ResCRUD DeviceR)

data Request = ReqDevice (ReqCRUD DeviceR) deriving (Generic)
             -- | ReqFamily (ReqCRUD FamilyR)

instance ToJSON Request
instance FromJSON Request

data Response = ResDevice (ResCRUD DeviceR) deriving (Generic)
             -- | ReqFamily (ReqCRUD FamilyR)

instance ToJSON Response
instance FromJSON Response

class IsResource r where
  type Identifier r :: *

  type CreateData r :: *
  type CreatedData r :: *

  type UpdateData r :: *
  type UpdatedData r :: *

  type ReadData r :: *
  type DidReadData r :: *

-- Device:

data DeviceR

data FamilyDevice = FamilyDevice { _deviceName :: Text
                                 , _deviceId :: Text
                                 } deriving (Generic)

instance FromJSON FamilyDevice
instance ToJSON FamilyDevice

$(makeClassy 'FamilyDevice)

data Device = Device { __familyDevice :: FamilyDevice
                     , _deviceKey :: Text
                     } deriving Generic

instance FromJSON Device
instance ToJSON Device

$(makeClassy 'Device)

instance HasFamilyDevice Device where
  familyDevice = _familyDevice

newtype DeviceId = DeviceId Int deriving (FromJSON, ToJSON)

data DeviceUpdate = DeviceSetName Text
                  | DeviceSetId Text
                  deriving Generic

instance ToJSON DeviceUpdate
instance FromJSON DeviceUpdate

data DeviceRead = DeviceReadFamily
                | DeviceReadAccount
                deriving Generic

instance ToJSON DeviceRead
instance FromJSON DeviceRead

data DeviceDidRead = DeviceDidReadFamily FamilyDevice
                   | DeviceDidReadAccount Device
                   deriving Generic

instance ToJSON DeviceDidRead
instance FromJSON DeviceDidRead

instance IsResource DeviceR where
  type Identifier DeviceR = DeviceId

  type CreateData DeviceR = ()
  type CreatedData DeviceR = ()

  type UpdateData DeviceR = DeviceUpdate
  type UpdatedData DeviceR = ()

  type ReadData DeviceR = DeviceRead
  type DidReadData DeviceR = DeviceDidRead



issueRequests :: [Request]
issueRequests = [ ReqDevice $ Create ()
                , ReqDevice $ Read (DeviceId 9) DeviceReadFamily
                , ReqDevice $ Read (DeviceId 9) DeviceReadAccount
                , ReqDevice $ Update (DeviceId 9) (DeviceSetName "huhu")
                , ReqDevice $ Update (DeviceId 9) (DeviceSetId "99")
                , ReqDevice $ Delete (DeviceId 9)
                ]
