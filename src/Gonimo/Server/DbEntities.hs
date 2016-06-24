{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Server.DbEntities where



import           Database.Persist.TH

import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)
-- import Data.Aeson.Types (FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToJSON)

import           Gonimo.Types          

share [mkPersist sqlSettings,  mkMigrate "migrateAll"] [persistLowerCase|
  Account
    created UTCTime
    email Text Maybe
    phone Text Maybe
    password Text Maybe

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
    secret Secret
    accountId AccountId
    lastAccessed UTCTime
    SecretClient secret
|]

instance FromJSON Invitation
instance ToJSON Invitation where
  toJSON = genericToJSON defaultOptions
--  toEncoding = genericToEncoding defaultOptions

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic
instance FromJSON SendInvitation
