{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Gonimo.Server.DbTypes where

import Database.Persist
import Database.Persist.TH
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson.Types (FromJSON)
import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)

type EmailAddress = Text
data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Generic)

derivePersistField "InvitationDelivery"


                        

instance FromJSON InvitationDelivery
instance ToJSON InvitationDelivery

