{-|
Module      : Gonimo.SocketAPI.Invitation.Internal
Description : Types and internal functions for "Gonimo.SocketAPI.Invitation"
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.SocketAPI.Invitation.Internal where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

import Gonimo.SocketAPI.Internal (DbKey)
import Gonimo.Types


newtype InvitationId = InvitationId DbKey deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)




data InvitationReply = InvitationAccept | InvitationReject deriving (Generic, Show, Eq, Ord)

instance FromJSON InvitationReply
instance ToJSON InvitationReply where
  toEncoding = genericToEncoding defaultOptions

type EmailAddress = Text

data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON InvitationDelivery

instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data SendInvitation = SendInvitation InvitationId InvitationDelivery deriving (Eq, Ord, Generic, Show)

instance FromJSON SendInvitation
instance ToJSON SendInvitation where
  toEncoding = genericToEncoding defaultOptions



-- | Invitation code data type for use in code based invitation.
newtype InvitationCode = InvitationCode Text deriving (Show, Read, FromJSON, ToJSON, Eq, Ord)

-- | TODO: More type safety for InvitationSecret, roadmap:
--   1. [x] Introduce type synonym InvitationSecret
--   2. [ ] Use InvitationSecret everywhere instead of Secret (where applicable)
--   3. [ ] Make it a newtype and fix breaking code
type InvitationSecret = Secret
