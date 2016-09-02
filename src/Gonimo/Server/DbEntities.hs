{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}

module Gonimo.Server.DbEntities where



import           Database.Persist.TH


import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToJSON, genericToEncoding)
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
    senderId ClientId -- Use ClientId here as it is the most precise information about the sender.
    receiverId AccountId Maybe  -- To be set on put InvitationInfo
    SecretInvitation secret
    deriving Show Generic

  Client -- think of "device", or more precise: a browser on a device
    name Text
    authToken AuthToken
    accountId AccountId
    lastAccessed UTCTime
    userAgent Text
    AuthTokenClient authToken
    deriving Generic

  User
    login Text
    password Text
    accountId AccountId
    AccountIdUser accountId

  FunnyWord
    word Text
    wordType FunnyWordType
    deriving Show
|]

instance FromJSON Invitation
instance ToJSON Invitation where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Family
instance ToJSON Family where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
