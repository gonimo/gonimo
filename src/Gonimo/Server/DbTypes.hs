{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- Needed for toText, fromText for BackendKey:
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gonimo.Server.DbTypes where


import Data.Aeson.Types (FromJSON, ToJSON(..), defaultOptions, genericToEncoding, genericToJSON)

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql 
import Database.Persist.TH
import GHC.Generics (Generic)
import Servant.Common.Text (FromText(..), ToText(..))

type EmailAddress = Text
data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Generic)

derivePersistField "InvitationDelivery"

instance FromJSON InvitationDelivery
instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions


instance ToText (BackendKey SqlBackend) where
  toText = toText . unSqlBackendKey

instance FromText (BackendKey SqlBackend) where
  fromText = fmap SqlBackendKey . fromText
