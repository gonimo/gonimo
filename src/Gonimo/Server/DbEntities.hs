{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Gonimo.Server.DbEntities where



import Database.Persist.TH

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gonimo.Server.DbTypes
-- import Data.Aeson.Types (FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)
import Data.Aeson.Types (FromJSON, ToJSON(..), defaultOptions, genericToJSON)

share [mkPersist sqlSettings,  mkMigrate "migrateAll"] [persistLowerCase|
  Account
    secret Secret
    created UTCTime
    lastAccessed UTCTime
    email Text Maybe
    phone Text Maybe
    password Text Maybe
    SecretAccount secret

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
|]

instance FromJSON Invitation
instance ToJSON Invitation where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions
