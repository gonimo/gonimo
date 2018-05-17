{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
import           Data.Text (Text)
import           GHC.Generics (Generic)
import Data.Aeson
import Control.Lens

{--

  Idea for a proper CRUD system, with structure.

  This would also enable us to properly implement multiple views on the same data.
  E.g. a family view and an account view, with the family view leaving out some information, like keys.

  Code would simply issue a notification for a changed resource, but clients will continue to subscribe concrete GET/Read requests. If a notification happens the resource will be mapped to all read commands that needs to be re-executed, like so:

  @
     data Resource = DeviceR (Identifier DeviceR) | FamilyR (Identifier 'FamilyR)

     class HasReadRequests resource readRequest where
       getReadRequests :: resource -> [readRequest]

     instance HasReadRequests Resource Request where
       getReadRequest (DeviceR devId) = ReqDevice <$> [ Get devId DeviceGetFamily
                                                      , Get devId DeviceGetAccount
                                                      ]
  @

--}



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

data DeviceRead = DeviceFamilyRead
                | DeviceAccountRead
                deriving Generic

instance ToJSON DeviceRead
instance FromJSON DeviceRead

data DeviceDidRead = DeviceFamilyDidRead FamilyDevice
                   | DeviceAccountDidRead Device
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
                , ReqDevice $ Read (DeviceId 9) DeviceFamilyRead
                , ReqDevice $ Read (DeviceId 9) DeviceAccountRead
                , ReqDevice $ Update (DeviceId 9) (DeviceSetName "huhu")
                , ReqDevice $ Update (DeviceId 9) (DeviceSetId "99")
                , ReqDevice $ Delete (DeviceId 9)
                ]

data Resource = DeviceR (Identifier DeviceR) -- | FamilyR (Identifier FamilyR)

-- | Mapping from resources to actual read requests which can be subscribed by clients.
class HasReadRequests resource readRequest where
  getReadRequests :: resource -> [readRequest]

instance HasReadRequests Resource Request where
  getReadRequests (DeviceR devId) = ReqDevice <$> [ Read devId DeviceFamilyRead
                                                  , Read devId DeviceAccountRead
                                                  ]


data ReqCRUD r = Create (CreateData r)
               | Read (RequestIdentifier r) (ReadData r)
               | Update (Identifier r) (UpdateData r)
               | Delete (Identifier r) deriving (Generic)

instance ToJSON (ReqCRUD DeviceR)
instance FromJSON (ReqCRUD DeviceR)

data ResCRUD r = Created (Identifier r) (CreatedData r)
               | DidRead (RequestIdentifier r) (DidReadData r)
               | Updated (Identifier r) (UpdatedData r)
               | Deleted (Identifier r)
               deriving Generic

instance ToJSON (ResCRUD DeviceR)
instance FromJSON (ResCRUD DeviceR)

data Request = ReqDevice (ReqCRUD DeviceR)
            deriving Generic
             -- | ReqFamily (ReqCRUD FamilyR)

instance ToJSON Request
instance FromJSON Request

data Response = ResDevice (ResCRUD DeviceR) deriving (Generic)
             -- | ReqFamily (ReqCRUD FamilyR)

instance ToJSON Response
instance FromJSON Response

class IsResource r where
  type Identifier r :: *

  -- | You can override this in order to have a different identifier for reads as for updates/deletes.
  type RequestIdentifier r :: *
  -- | By default this is just the normal 'Identifier'
  type instance RequestIdentifier r = Identifier r

  type CreateData r :: *
  type CreatedData r :: *

  type UpdateData r :: *
  type UpdatedData r :: *

  type ReadData r :: *
  type DidReadData r :: *
