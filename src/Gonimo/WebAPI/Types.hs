{-# LANGUAGE FlexibleInstances #-}
-- Types which are only relevant to the Client.
-- They are either accepted from the client or sent to the client but have no further use
-- on the server side.

module Gonimo.WebAPI.Types where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

import Gonimo.Server.Types
import Gonimo.Server.DbEntities


data AuthData = AuthData {
    accountId :: AccountId
  , clientId :: ClientId
  , authToken :: AuthToken
  } deriving (Generic, Show)

instance ToJSON AuthData

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving Generic
instance FromJSON SendInvitation
instance ToJSON SendInvitation

