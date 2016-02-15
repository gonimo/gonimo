{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Gonimo.Server.Database where

import Database.Persist
import Database.Persist.TH
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

mkPersist sqlSettings [persistLowerCase|
  Account
    secret ByteString
    created UTCTime
    lastAccessed UTCTime
    email Text Maybe
    phone Text Maybe
    password Text Maybe
    AccountSecret secret
    AccountEmail email
    AccountPhone phone

  Family
    created UTCTime
    lastAccessed UTCTime
    creatorId AccountId

  FamilyAccount
    accountId AccountId
    familyId  FamilyId
    FamilyMember accountId familyId

  Invitation
    secret Text
    familyId FamilyId
    InvitationSecret secret
|]
