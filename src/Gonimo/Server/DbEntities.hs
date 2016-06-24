{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Server.DbEntities where



import           Database.Persist.TH


import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToJSON)
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)

import           Gonimo.Server.Types

share [mkPersist sqlSettings,  mkMigrate "migrateAll"] [persistLowerCase|
  Account
    created UTCTime
    deriving Generic

  Family
    name Text
    created UTCTime
    lastAccessed UTCTime

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
    SecretInvitation secret
    deriving Show Generic

  Client
    authToken AuthToken
    accountId AccountId
    lastAccessed UTCTime
    AuthTokenClient authToken
    deriving Generic

  User
    login Text
    password Text
    accountId AccountId

|]

instance FromJSON Invitation
instance ToJSON Invitation where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

