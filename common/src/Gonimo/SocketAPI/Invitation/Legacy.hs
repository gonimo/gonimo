{-|
Module      : Gonimo.SocketAPI.Invitation.Legacy
Description : Legacy/deprecated types.
Copyright   : (c) Robert Klotzner, 2018

Deprecated at March 21, 2018. Used in non CRUD invitation 'ServerRequest' constructors.
-}
module Gonimo.SocketAPI.Invitation.Legacy ( Invitation(..)
                                          , InvitationInfo(..)
                                          , module Invitation
                                          ) where


import           Data.Aeson
import           Data.Text                            (Text)
import           Data.Time                            (UTCTime)
import           GHC.Generics                         (Generic)

import           Gonimo.SocketAPI.Invitation.Internal as Invitation
import           Gonimo.SocketAPI.Types
import           Gonimo.Types
import           Gonimo.SocketAPI.Invitation as Invitation hiding (Invitation, InvitationInfo)



data Invitation
  = Invitation { invitationSecret     :: !InvitationSecret
               , invitationFamilyId   :: !FamilyId
               , invitationCreated    :: !UTCTime
               , invitationDelivery   :: !InvitationDelivery
               , invitationSenderId   :: !DeviceId
               , invitationReceiverId :: !(Maybe AccountId)
               }
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Invitation
instance ToJSON Invitation where
  toEncoding = genericToEncoding defaultOptions

data InvitationInfo = InvitationInfo {
    invitationInfoFamily        :: !FamilyName
  , invitationInfoSendingDevice :: !Text
  , invitationInfoSendingUser   :: !(Maybe Text)
  } deriving (Generic, Show, Ord, Eq)

instance FromJSON InvitationInfo
instance ToJSON InvitationInfo where
  toEncoding = genericToEncoding defaultOptions
