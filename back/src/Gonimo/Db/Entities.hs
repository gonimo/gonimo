{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Gonimo.Db.Entities where



import           Database.Persist.TH


import           Data.Aeson.Types        (FromJSON, ToJSON (..), defaultOptions,
                                          genericToEncoding, genericToJSON)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           GHC.Generics            (Generic)

import           Gonimo.Db.PersistFields ()
import           Gonimo.SocketAPI.Types  (InvitationDelivery)
import           Gonimo.Types            hiding (FamilyName)
import qualified Gonimo.Types            as Server

share [mkPersist (sqlSettings { mpsGeneric = False }),  mkMigrate "migrateAll"] [persistLowerCase|
  Account
    created UTCTime
    deriving Generic

  Family
    name Server.FamilyName
    created UTCTime
    lastAccessed UTCTime -- TODO: Should really by "lastModified", fix it once we have db migrations and smart client updates.
    lastUsedBabyNames [Text]
    deriving Generic Show

  FamilyAccount
    accountId AccountId
    familyId  FamilyId
    joined UTCTime
    invitedBy InvitationDelivery Maybe
    FamilyMember accountId familyId

  Invitation
    secret Secret
    familyId FamilyId
    created UTCTime
    delivery InvitationDelivery
    senderId DeviceId -- Use DeviceId here as it is the most precise information about the sender.
    receiverId AccountId Maybe  -- To be set on put InvitationInfo
    SecretInvitation secret
    deriving Show Generic

  Device -- or more precise: a browser on a device
    name Text Maybe
    authToken AuthToken
    accountId AccountId
    lastAccessed UTCTime
    userAgent Text
    AuthTokenDevice authToken
    deriving Generic

  User
    login Text
    password Text
    accountId AccountId
    AccountIdUser accountId
|]

instance FromJSON Invitation
instance ToJSON Invitation where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Family
instance ToJSON Family where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- instance ToJSVal (Key a)
-- instance FromJSVal (Key a)
-- instance ToJSString (Key a)
-- instance FromJSString (Key a)
-- deriving instance Generic (Key Device)
-- instance ToJSVal (Key Device)
-- instance FromJSVal (Key Device)
-- instance ToJSString (Key Device)
-- instance FromJSString (Key Device)
-- instance PToJSVal (Key Device)
-- instance PFromJSVal (Key Device)

-- deriving instance Generic (Key Account)
-- instance ToJSVal (Key Account)
-- instance FromJSVal (Key Account)
-- instance ToJSString (Key Account)
-- instance FromJSString (Key Account)
-- instance PToJSVal (Key Account)
-- instance PFromJSVal (Key Account)

-- deriving instance Generic (BackendKey SqlBackend)
-- instance ToJSVal (BackendKey SqlBackend) where
--   toJSVal = toJSVal . show . unSqlBackendKey

-- instance FromJSVal (BackendKey SqlBackend) where
--   fromJSVal val = runMaybeT $ do
--     t <- MaybeT $ fromJSVal val
--     intVal <- MaybeT . pure $ readMay t
--     pure $ SqlBackendKey intVal

