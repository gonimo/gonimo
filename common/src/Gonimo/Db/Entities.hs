{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Gonimo.Db.Entities where

import           Database.Persist.Sql
import           Database.Persist.TH  (lensPTH)

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import qualified Gonimo.Types         as Server
import Gonimo.Types                   hiding (FamilyName, familyName)

--------------------------------------------------------------------------------
--                                  ACCOUNT                                   --
--------------------------------------------------------------------------------

newtype Account = Account {accountCreated :: UTCTime} deriving (Generic)
type AccountId = Key Account

instance PersistEntity Account where
  type PersistEntityBackend Account = SqlBackend
  data Unique Account
  newtype Key Account = AccountKey {unAccountKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField Account typ = typ ~ Key Account => AccountId
                               | typ ~ UTCTime => AccountCreated
  keyToValues = pure . toPersistValue . unAccountKey
  keyFromValues = fmap AccountKey . fromPersistValue . headNote

  persistFieldDef AccountId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "AccountId") SqlInt64 [] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef AccountCreated = FieldDef (HaskellName "created") (DBName "created") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference

  entityDef _ = EntityDef (HaskellName "Account") (DBName "account")
                          (persistFieldDef AccountId) []
                          [persistFieldDef AccountCreated] [] [] ["Generic"]
                          mempty False

  toPersistFields (Account x) = [SomePersistField x]

  fromPersistValues [x] = Account <$> fromPersistValue x
  fromPersistValues x = let x' = T.pack $ show x
                         in Left $ "Account: fromPersistValues failed on: " <> x'

  persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
  persistUniqueToValues     _ = error "Degenerate case, should never happen"

  persistUniqueKeys (Account _) = []
  persistIdField = AccountId

  fieldLens AccountId      = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens AccountCreated = lensPTH (accountCreated . entityVal) (\(Entity key value) x -> Entity key (value {accountCreated = x}))

instance ToBackendKey SqlBackend Account where
  toBackendKey = unAccountKey
  fromBackendKey = AccountKey

--------------------------------------------------------------------------------
--                                    USER                                    --
--------------------------------------------------------------------------------

data User = User {userLogin :: !Text
                 ,userPassword :: !Text
                 ,userAccountId :: !(Key Account)}
type UserId = Key User
instance PersistEntity User where
  type PersistEntityBackend User = SqlBackend
  data Unique User = AccountIdUser (Key Account)
  newtype Key User = UserKey {unUserKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField User typ
    = typ ~ Key User => UserId
    | typ ~ Text => UserLogin
    | typ ~ Text => UserPassword
    | typ ~ Key Account => UserAccountId
  keyToValues = pure . toPersistValue . unUserKey
  keyFromValues = fmap UserKey . fromPersistValue . headNote
  persistFieldDef UserId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "UserId") SqlInt64 [] True (ForeignRef (HaskellName "User") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef UserLogin = FieldDef (HaskellName "login") (DBName "login") (FTTypeCon Nothing "Text") SqlString [] True NoReference
  persistFieldDef UserPassword = FieldDef (HaskellName "password") (DBName "password") (FTTypeCon Nothing "Text") SqlString [] True NoReference
  persistFieldDef UserAccountId = FieldDef (HaskellName "accountId") (DBName "account_id") (FTTypeCon Nothing "AccountId") SqlInt64 [] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
  entityDef _ = EntityDef (HaskellName "User") (DBName "user")
        (persistFieldDef UserId)
        []
        [persistFieldDef UserLogin
        ,persistFieldDef UserPassword
        ,persistFieldDef UserAccountId]
        [UniqueDef (HaskellName "AccountIdUser") (DBName "account_id_user") [(HaskellName "accountId", DBName "account_id")] []]
        []
        []
        mempty
        False
  toPersistFields (User x1 x2 x3)
    = [SomePersistField x1,
       SomePersistField x2,
       SomePersistField x3]
  fromPersistValues [x1, x2, x3]
    = User <$> fromPersistValue x1
           <*> fromPersistValue x2
           <*> fromPersistValue x3
  fromPersistValues x = Left $ "User: fromPersistValues failed on: " <> T.pack (show x)
  persistUniqueToFieldNames AccountIdUser{} = [(HaskellName "accountId", DBName "account_id")]
  persistUniqueToValues (AccountIdUser x) = [toPersistValue x]
  persistUniqueKeys (User _login _password _accountId) = [AccountIdUser _accountId]
  persistIdField = UserId
  fieldLens UserId        = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens UserLogin     = lensPTH (userLogin     . entityVal) (\ (Entity key value) x -> Entity key (value {userLogin = x}))
  fieldLens UserPassword  = lensPTH (userPassword  . entityVal) (\ (Entity key value) x -> Entity key (value {userPassword = x}))
  fieldLens UserAccountId = lensPTH (userAccountId . entityVal) (\ (Entity key value) x -> Entity key (value {userAccountId = x}))
instance ToBackendKey SqlBackend User where
  toBackendKey = unUserKey
  fromBackendKey = UserKey

--------------------------------------------------------------------------------
--                                  FAMILY                                    --
--------------------------------------------------------------------------------

data Family = Family {familyName :: !Server.FamilyName
                     ,familyCreated :: !UTCTime
                     ,familyLastAccessed :: !UTCTime
                     ,familyLastUsedBabyNames :: ![Text]
                     } deriving (Generic, Show)

instance FromJSON Family
instance ToJSON Family

type FamilyId = Key Family
instance PersistEntity Family where
  type PersistEntityBackend Family = SqlBackend
  data Unique Family
  newtype Key Family = FamilyKey {unFamilyKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField Family typ
    = typ ~ Key Family => FamilyId
    | typ ~ Server.FamilyName => FamilyName
    | typ ~ UTCTime => FamilyCreated
    | typ ~ UTCTime => FamilyLastAccessed
    | typ ~ [Text] => FamilyLastUsedBabyNames
  keyToValues = pure . toPersistValue . unFamilyKey
  keyFromValues = fmap FamilyKey . fromPersistValue . headNote

  persistFieldDef FamilyId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "FamilyId") SqlInt64 [] True (ForeignRef (HaskellName "Family") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef FamilyName = FieldDef (HaskellName "name") (DBName "name") (FTTypeCon (Just "Server") "FamilyName") SqlString [] True NoReference
  persistFieldDef FamilyCreated = FieldDef (HaskellName "created") (DBName "created") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
  persistFieldDef FamilyLastAccessed = FieldDef (HaskellName "lastAccessed") (DBName "last_accessed") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
  persistFieldDef FamilyLastUsedBabyNames = FieldDef (HaskellName "lastUsedBabyNames") (DBName "last_used_baby_names") (FTList (FTTypeCon Nothing "Text")) SqlString [] True NoReference

  entityDef _ = EntityDef (HaskellName "Family") (DBName "family")
        (persistFieldDef FamilyId)
        []
        [persistFieldDef FamilyName
        ,persistFieldDef FamilyCreated
        ,persistFieldDef FamilyLastAccessed
        ,persistFieldDef FamilyLastUsedBabyNames]
        []
        []
        ["Generic", "Show"]
        mempty
        False
  toPersistFields (Family x1 x2 x3 x4)
    = [SomePersistField x1, SomePersistField x2,
       SomePersistField x3, SomePersistField x4]
  fromPersistValues [x1, x2, x3, x4]
    = Family <$> fromPersistValue x1
             <*> fromPersistValue x2
             <*> fromPersistValue x3
             <*> fromPersistValue x4
  fromPersistValues x
    = let x' = T.pack $ show x
       in Left $ "Family: fromPersistValues failed on: " <> x'
  persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
  persistUniqueToValues     _ = error "Degenerate case, should never happen"
  persistUniqueKeys Family{} = []

  persistIdField = FamilyId

  fieldLens FamilyId = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens FamilyName = lensPTH (familyName . entityVal) (\ (Entity key value) x -> Entity key (value {familyName = x}))
  fieldLens FamilyCreated = lensPTH (familyCreated . entityVal) (\ (Entity key value) x -> Entity key (value {familyCreated = x}))
  fieldLens FamilyLastAccessed = lensPTH (familyLastAccessed . entityVal) (\ (Entity key value) x -> Entity key (value {familyLastAccessed = x}))
  fieldLens FamilyLastUsedBabyNames = lensPTH (familyLastUsedBabyNames . entityVal) (\ (Entity key value) x -> Entity key (value {familyLastUsedBabyNames = x}))
instance ToBackendKey SqlBackend Family where
  toBackendKey = unFamilyKey
  fromBackendKey = FamilyKey

--------------------------------------------------------------------------------
--                                INVITATION                                  --
--------------------------------------------------------------------------------

data Invitation
  = Invitation {invitationSecret :: !Secret,
                invitationFamilyId :: !(Key Family),
                invitationCreated :: !UTCTime,
                invitationDelivery :: !InvitationDelivery,
                invitationSenderId :: !(Key Device),
                invitationReceiverId :: !(Maybe (Key Account))}
  deriving (Show, Generic)

instance FromJSON Invitation
instance ToJSON Invitation

type InvitationId = Key Invitation
instance PersistEntity Invitation where
  type PersistEntityBackend Invitation = SqlBackend
  data Unique Invitation = SecretInvitation Secret
  newtype Key Invitation = InvitationKey {unInvitationKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField Invitation typ
    = typ ~ Key Invitation => InvitationId
    | typ ~ Secret => InvitationSecret
    | typ ~ Key Family => InvitationFamilyId
    | typ ~ UTCTime => InvitationCreated
    | typ ~ InvitationDelivery => InvitationDelivery
    | typ ~ Key Device => InvitationSenderId
    | typ ~ Maybe (Key Account) => InvitationReceiverId
  keyToValues = pure . toPersistValue . unInvitationKey
  keyFromValues = fmap InvitationKey . fromPersistValue . headNote
  persistFieldDef InvitationId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "InvitationId") SqlInt64 [] True (ForeignRef (HaskellName "Invitation") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef InvitationSecret = FieldDef (HaskellName "secret") (DBName "secret") (FTTypeCon Nothing "Secret") SqlString [] True NoReference
  persistFieldDef InvitationFamilyId = FieldDef (HaskellName "familyId") (DBName "family_id") (FTTypeCon Nothing "FamilyId") SqlInt64 [] True (ForeignRef (HaskellName "Family") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef InvitationCreated = FieldDef (HaskellName "created") (DBName "created") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
  persistFieldDef InvitationDelivery = FieldDef (HaskellName "delivery") (DBName "delivery") (FTTypeCon Nothing "InvitationDelivery") SqlString [] True NoReference
  persistFieldDef InvitationSenderId = FieldDef (HaskellName "senderId") (DBName "sender_id") (FTTypeCon Nothing "DeviceId") SqlInt64 [] True (ForeignRef (HaskellName "Device") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef InvitationReceiverId = FieldDef (HaskellName "receiverId") (DBName "receiver_id") (FTTypeCon Nothing "AccountId") SqlInt64 ["Maybe"] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
  entityDef _ = EntityDef (HaskellName "Invitation") (DBName "invitation")
        (persistFieldDef InvitationId)
        []
        [persistFieldDef InvitationSecret
        ,persistFieldDef InvitationFamilyId
        ,persistFieldDef InvitationCreated
        ,persistFieldDef InvitationDelivery
        ,persistFieldDef InvitationSenderId
        ,persistFieldDef InvitationReceiverId]
        [UniqueDef (HaskellName "SecretInvitation") (DBName "secret_invitation")
                  [(HaskellName "secret", DBName "secret")] []]
        []
        ["Show", "Generic"]
        mempty
        False
  toPersistFields (Invitation s f c d s' r)
    = [SomePersistField s ,SomePersistField f ,SomePersistField c ,SomePersistField d ,SomePersistField s' ,SomePersistField r]
  fromPersistValues
    [x1, x2, x3, x4, x5, x6]
    = Invitation
      <$> fromPersistValue x1
      <*> fromPersistValue x2
      <*> fromPersistValue x3
      <*> fromPersistValue x4
      <*> fromPersistValue x5
      <*> fromPersistValue x6
  fromPersistValues x
    = let x' = T.pack $ show x
       in Left $ "Invitation: fromPersistValues failed on: " <> x'

  persistUniqueToFieldNames SecretInvitation{}
    = [(HaskellName "secret", DBName "secret")]
  persistUniqueToValues (SecretInvitation x) = [toPersistValue x]
  persistUniqueKeys (Invitation _secret _ _ _ _ _)
    = [SecretInvitation _secret]

  persistIdField = InvitationId

  fieldLens InvitationId = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens InvitationSecret = lensPTH (invitationSecret . entityVal) (\ (Entity key value) x -> Entity key (value {invitationSecret = x}))
  fieldLens InvitationFamilyId = lensPTH (invitationFamilyId . entityVal) (\ (Entity key value) x -> Entity key (value {invitationFamilyId = x}))
  fieldLens InvitationCreated = lensPTH (invitationCreated . entityVal) (\ (Entity key value) x -> Entity key (value {invitationCreated = x}))
  fieldLens InvitationDelivery = lensPTH (invitationDelivery . entityVal) (\ (Entity key value) x -> Entity key (value {invitationDelivery = x}))
  fieldLens InvitationSenderId = lensPTH (invitationSenderId . entityVal) (\ (Entity key value) x -> Entity key (value {invitationSenderId = x}))
  fieldLens InvitationReceiverId = lensPTH (invitationReceiverId . entityVal) (\ (Entity key value) x -> Entity key (value {invitationReceiverId = x}))
instance ToBackendKey SqlBackend Invitation where
  toBackendKey = unInvitationKey
  fromBackendKey = InvitationKey


--------------------------------------------------------------------------------
--                                  FAMILY                                    --
--------------------------------------------------------------------------------

data FamilyAccount
  = FamilyAccount {familyAccountAccountId :: !(Key Account),
                   familyAccountFamilyId :: !(Key Family),
                   familyAccountJoined :: !UTCTime,
                   familyAccountInvitedBy :: !(Maybe InvitationDelivery)}

type FamilyAccountId = Key FamilyAccount
instance PersistEntity FamilyAccount where
  type PersistEntityBackend FamilyAccount = SqlBackend
  data Unique FamilyAccount = FamilyMember (Key Account) (Key Family)
  newtype Key FamilyAccount = FamilyAccountKey {unFamilyAccountKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField FamilyAccount typ
    = typ ~ Key FamilyAccount => FamilyAccountId
    | typ ~ Key Account => FamilyAccountAccountId
    | typ ~ Key Family => FamilyAccountFamilyId
    | typ ~ UTCTime => FamilyAccountJoined
    | typ ~ Maybe InvitationDelivery => FamilyAccountInvitedBy
  keyToValues = pure . toPersistValue . unFamilyAccountKey
  keyFromValues = fmap FamilyAccountKey . fromPersistValue . headNote

  persistFieldDef FamilyAccountId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "FamilyAccountId") SqlInt64 [] True (ForeignRef (HaskellName "FamilyAccount") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef FamilyAccountAccountId = FieldDef (HaskellName "accountId") (DBName "account_id") (FTTypeCon Nothing "AccountId") SqlInt64 [] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef FamilyAccountFamilyId = FieldDef (HaskellName "familyId") (DBName "family_id") (FTTypeCon Nothing "FamilyId") SqlInt64 [] True (ForeignRef (HaskellName "Family") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef FamilyAccountJoined = FieldDef (HaskellName "joined") (DBName "joined") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
  persistFieldDef FamilyAccountInvitedBy = FieldDef (HaskellName "invitedBy") (DBName "invited_by") (FTTypeCon Nothing "InvitationDelivery") SqlString ["Maybe"] True NoReference

  entityDef _ = EntityDef (HaskellName "FamilyAccount") (DBName "family_account")
        (persistFieldDef FamilyAccountId)
        []
        [persistFieldDef FamilyAccountAccountId
        ,persistFieldDef FamilyAccountFamilyId
        ,persistFieldDef FamilyAccountJoined
        ,persistFieldDef FamilyAccountInvitedBy ]
        [UniqueDef (HaskellName "FamilyMember") (DBName "family_member") [(HaskellName "accountId", DBName "account_id"), (HaskellName "familyId", DBName "family_id")] []]
        [] [] mempty False
  toPersistFields
    (FamilyAccount a f j i)
    = [SomePersistField a,
       SomePersistField f,
       SomePersistField j,
       SomePersistField i]
  fromPersistValues [x1, x2, x3, x4]
    = FamilyAccount
      <$> fromPersistValue x1
      <*> fromPersistValue x2
      <*> fromPersistValue x3
      <*> fromPersistValue x4
  fromPersistValues x
    = let x' = T.pack $ show x
       in Left $ "FamilyAccount: fromPersistValues failed on: " <> x'
  persistUniqueToFieldNames FamilyMember{}
    = [(HaskellName "accountId", DBName "account_id"),
       (HaskellName "familyId", DBName "family_id")]
  persistUniqueToValues (FamilyMember x1 x2)
    = [toPersistValue x1, toPersistValue x2]
  persistUniqueKeys (FamilyAccount _accountId _familyId _joined _invitedBy)
    = [FamilyMember _accountId _familyId]

  persistIdField = FamilyAccountId

  fieldLens FamilyAccountId = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens FamilyAccountAccountId = lensPTH (familyAccountAccountId . entityVal) (\ (Entity key value) x -> Entity key (value {familyAccountAccountId = x}))
  fieldLens FamilyAccountFamilyId = lensPTH (familyAccountFamilyId . entityVal) (\ (Entity key value) x -> Entity key (value {familyAccountFamilyId = x}))
  fieldLens FamilyAccountJoined = lensPTH (familyAccountJoined . entityVal) (\ (Entity key value) x -> Entity key (value {familyAccountJoined = x}))
  fieldLens FamilyAccountInvitedBy = lensPTH (familyAccountInvitedBy . entityVal) (\ (Entity key value) x -> Entity key (value {familyAccountInvitedBy = x}))

instance ToBackendKey SqlBackend FamilyAccount where
  toBackendKey = unFamilyAccountKey
  fromBackendKey = FamilyAccountKey

--------------------------------------------------------------------------------
--                                  DEVICE                                    --
--------------------------------------------------------------------------------

data Device
  = Device {deviceName :: !(Maybe Text),
            deviceAuthToken :: !AuthToken,
            deviceAccountId :: !(Key Account),
            deviceLastAccessed :: !UTCTime,
            deviceUserAgent :: !Text}
  deriving (Generic)
type DeviceId = Key Device
instance PersistEntity Device where
  type PersistEntityBackend Device = SqlBackend
  data Unique Device = AuthTokenDevice AuthToken
  newtype Key Device = DeviceKey {unDeviceKey :: BackendKey SqlBackend}
    deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql, ToJSON, FromJSON)
  data EntityField Device typ
    = typ ~ Key Device => DeviceId
    | typ ~ Maybe Text => DeviceName
    | typ ~ AuthToken => DeviceAuthToken
    | typ ~ Key Account => DeviceAccountId
    | typ ~ UTCTime => DeviceLastAccessed
    | typ ~ Text => DeviceUserAgent
  keyToValues = pure . toPersistValue . unDeviceKey
  keyFromValues = fmap DeviceKey . fromPersistValue . headNote
  entityDef _
    = EntityDef (HaskellName "Device") (DBName "device")
        (FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "DeviceId") SqlInt64 [] True (ForeignRef (HaskellName "Device") (FTTypeCon (Just "Data.Int") "Int64")))
        []
        [FieldDef (HaskellName "name") (DBName "name") (FTTypeCon Nothing "Text") SqlString ["Maybe"] True NoReference
        ,FieldDef (HaskellName "authToken") (DBName "auth_token") (FTTypeCon Nothing "AuthToken") SqlString [] True NoReference
        ,FieldDef (HaskellName "accountId") (DBName "account_id") (FTTypeCon Nothing "AccountId") SqlInt64 [] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
        ,FieldDef (HaskellName "lastAccessed") (DBName "last_accessed") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
        ,FieldDef (HaskellName "userAgent") (DBName "user_agent") (FTTypeCon Nothing "Text") SqlString [] True NoReference]
        [UniqueDef (HaskellName "AuthTokenDevice") (DBName "auth_token_device") [(HaskellName "authToken", DBName "auth_token")] []]
        [] ["Generic"] mempty False
  toPersistFields
    (Device x1 x2 x3 x4 x5)
    = [SomePersistField x1,
       SomePersistField x2,
       SomePersistField x3,
       SomePersistField x4,
       SomePersistField x5]
  fromPersistValues
    [x1, x2, x3, x4, x5]
    = Device
      <$> fromPersistValue x1
      <*> fromPersistValue x2
      <*> fromPersistValue x3
      <*> fromPersistValue x4
      <*> fromPersistValue x5
  fromPersistValues x
    = let x' = T.pack $ show x
       in Left $ "Device: fromPersistValues failed on: " <> x'
  persistUniqueToFieldNames AuthTokenDevice{}
    = [(HaskellName "authToken", DBName "auth_token")]
  persistUniqueToValues (AuthTokenDevice x)
    = [toPersistValue x]
  persistUniqueKeys (Device _ _authToken _ _ _) = [AuthTokenDevice _authToken]
  persistFieldDef DeviceId = FieldDef (HaskellName "Id") (DBName "id") (FTTypeCon Nothing "DeviceId") SqlInt64 [] True (ForeignRef (HaskellName "Device") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef DeviceName = FieldDef (HaskellName "name") (DBName "name") (FTTypeCon Nothing "Text") SqlString ["Maybe"] True NoReference
  persistFieldDef DeviceAuthToken = FieldDef (HaskellName "authToken") (DBName "auth_token") (FTTypeCon Nothing "AuthToken") SqlString [] True NoReference
  persistFieldDef DeviceAccountId = FieldDef (HaskellName "accountId") (DBName "account_id") (FTTypeCon Nothing "AccountId") SqlInt64 [] True (ForeignRef (HaskellName "Account") (FTTypeCon (Just "Data.Int") "Int64"))
  persistFieldDef DeviceLastAccessed = FieldDef (HaskellName "lastAccessed") (DBName "last_accessed") (FTTypeCon Nothing "UTCTime") SqlDayTime [] True NoReference
  persistFieldDef DeviceUserAgent = FieldDef (HaskellName "userAgent") (DBName "user_agent") (FTTypeCon Nothing "Text") SqlString [] True NoReference
  persistIdField = DeviceId
  fieldLens DeviceId = lensPTH entityKey (\ (Entity _ value) key -> Entity key value)
  fieldLens DeviceName = lensPTH (deviceName . entityVal) (\ (Entity key value) x -> Entity key (value {deviceName = x}))
  fieldLens DeviceAuthToken = lensPTH (deviceAuthToken . entityVal) (\ (Entity key value) x -> Entity key (value {deviceAuthToken = x}))
  fieldLens DeviceAccountId = lensPTH (deviceAccountId . entityVal) (\ (Entity key value) x -> Entity key (value {deviceAccountId = x}))
  fieldLens DeviceLastAccessed = lensPTH (deviceLastAccessed . entityVal) (\ (Entity key value) x -> Entity key (value {deviceLastAccessed = x}))
  fieldLens DeviceUserAgent = lensPTH (deviceUserAgent . entityVal) (\ (Entity key value) x -> Entity key (value {deviceUserAgent = x}))
instance ToBackendKey SqlBackend Device where
  toBackendKey = unDeviceKey
  fromBackendKey = DeviceKey

--------------------------------------------------------------------------------
--                               MIGRATE ALL                                  --
--------------------------------------------------------------------------------

migrateAll :: Migration
migrateAll = let defs :: [EntityDef]
                 defs = [entityDef (Nothing :: Maybe Account)
                        ,entityDef (Nothing :: Maybe Family)
                        ,entityDef (Nothing :: Maybe FamilyAccount)
                        ,entityDef (Nothing :: Maybe Invitation)
                        ,entityDef (Nothing :: Maybe Device)
                        ,entityDef (Nothing :: Maybe User)]
              in mapM_ (migrate defs) defs

--------------------------------------------------------------------------------

headNote :: Show a => [a] -> a
headNote [x] = x
headNote xs = error $ "expected a list of one element, but got: " <> show xs

