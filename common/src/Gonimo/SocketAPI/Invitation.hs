{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Gonimo.SocketAPI.Invitation
Description : Invitation CRUD interface
Copyright   : (c) Robert Klotzner, 2018

This module is intended to be imported qualifed as Invitation.
-}
module Gonimo.SocketAPI.Invitation ( -- * Types & classes
                                     InvitationId
                                   , Invitation(..)
                                   , HasInvitation(..)
                                   , InvitationInfo(..)
                                   , HasInvitationInfo(..)
                                   , InvitationReply(..)
                                   , InvitationDelivery(..)
                                   , SendInvitation(..)
                                   , InvitationCode(..)
                                   , InvitationSecret
                                   -- * CRUD
                                   , InvitationR
                                   , Read(..)
                                   , DidRead(..)
                                   , ReadBy(..)
                                   , Update(..)
                                   , Updated(..)
                                   ) where


import           Control.Lens
import           Data.Aeson
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import           Prelude hiding (Read)
import           Data.Time        (UTCTime)

import           Gonimo.SocketAPI.Invitation.Internal
import           Gonimo.SocketAPI.Types
import           Gonimo.Types
import           Gonimo.SocketAPI.CRUD

-- | Type for retrieving CRUD interface of this module.
data InvitationR

instance ToJSON (ReqCRUD InvitationR) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (ReqCRUD InvitationR)
instance ToJSON (ResCRUD InvitationR) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (ResCRUD InvitationR)

deriving instance Show (ReqCRUD InvitationR)
deriving instance Eq (ReqCRUD InvitationR)
deriving instance Ord (ReqCRUD InvitationR)

deriving instance Show (ResCRUD InvitationR)
deriving instance Eq (ResCRUD InvitationR)
deriving instance Ord (ResCRUD InvitationR)

-- | Full 'Invitation' data as retrieved by 'Read'.
data Invitation
  = Invitation { _secret     :: !InvitationSecret
               , _familyId   :: !FamilyId
               , _created    :: !UTCTime
               , _delivery   :: !InvitationDelivery
               , _senderId   :: !DeviceId
               , _receiverId :: !(Maybe AccountId)
               , _code       :: !(Maybe InvitationCode)
               }
  deriving (Show, Generic, Eq, Ord)


instance FromJSON Invitation
instance ToJSON Invitation where
  toEncoding = genericToEncoding defaultOptions

-- | Invitation info as seen by a claiming device.
--
--   If a device claims an invitation, this is the information it will get.
--   You will receive 'DidReadInfo' with 'InvitationInfo' when issuing 'ReadInfo'.
data InvitationInfo = InvitationInfo {
    _infoFamily        :: !FamilyName
  , _infoSendingDevice :: !Text
  , _infoSendingUser   :: !(Maybe Text)
  , _infoSecret        :: !InvitationSecret
  } deriving (Generic, Show, Ord, Eq)

instance FromJSON InvitationInfo
instance ToJSON InvitationInfo where
  toEncoding = genericToEncoding defaultOptions



-- | Invitations are somewhat special, as they can be retrieved by
--
--   - their id obviously
--   - their secret
--   - and if available a short code, meant for humans to type.
--
--   For updates and deletions the id has to be specified.
data ReadBy = ReadById     InvitationId
            | ReadBySecret InvitationSecret
            | ReadByCode   InvitationCode
            deriving (Generic, Eq, Ord, Show)

instance ToJSON ReadBy where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ReadBy

-- | Used as 'ReadData', for specifying what view to read.
data Read = ReadFull
          | ReadInfo
          deriving (Generic, Eq, Ord, Show)

instance ToJSON Read where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Read

-- | Used as 'DidReadData' in response to the corresponding 'Read' request.
data DidRead = DidReadFull Invitation
             | DidReadInfo InvitationInfo
             deriving (Generic, Eq, Ord, Show)

instance ToJSON DidRead where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON DidRead

-- | Used as 'UpdateData' for specifying what part of the invitation to update.
data Update = UpdateDelivery InvitationDelivery
              -- | Claim an invitation and retrieve the info.
              --
              --  This call is mandatory for accessing invitation info for an
              --  invited device.
            | UpdateReceiver AccountId
            | UpdateCode
            | DeleteCode
            deriving (Generic, Eq, Ord, Show)

instance ToJSON Update where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Update

data Updated = UpdatedDelivery InvitationDelivery
               -- | Update the receiver.
               --
               --   The invitation is now claimed and can be accepted or denied.
             | UpdatedReceiver AccountId InvitationInfo
             | UpdatedCode InvitationCode
             | DeletedCode
             deriving (Generic, Eq, Ord, Show)

instance ToJSON Updated where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Updated

instance IsResource InvitationR where
  -- | For updates and deletes we only accept the id.
  type Identifier InvitationR     = InvitationId
  type RequestIdentifier InvitationR = ReadBy

  type CreateData InvitationR     = FamilyId
  type CreatedData InvitationR    = ()

  type UpdateData InvitationR     = Update
  type UpdatedData InvitationR    = Updated

  type ReadData InvitationR       = Read
  type DidReadData InvitationR    = DidRead


-- Auto generated lenses:

class HasInvitation a where
  invitation :: Lens' a Invitation

  secret :: Lens' a InvitationSecret
  secret = invitation . go
    where
      go :: Lens' Invitation InvitationSecret
      go f invitation' = (\secret' -> invitation' { _secret = secret' }) <$> f (_secret invitation')


  familyId :: Lens' a FamilyId
  familyId = invitation . go
    where
      go :: Lens' Invitation FamilyId
      go f invitation' = (\familyId' -> invitation' { _familyId = familyId' }) <$> f (_familyId invitation')


  created :: Lens' a UTCTime
  created = invitation . go
    where
      go :: Lens' Invitation UTCTime
      go f invitation' = (\created' -> invitation' { _created = created' }) <$> f (_created invitation')


  delivery :: Lens' a InvitationDelivery
  delivery = invitation . go
    where
      go :: Lens' Invitation InvitationDelivery
      go f invitation' = (\delivery' -> invitation' { _delivery = delivery' }) <$> f (_delivery invitation')


  senderId :: Lens' a DeviceId
  senderId = invitation . go
    where
      go :: Lens' Invitation DeviceId
      go f invitation' = (\senderId' -> invitation' { _senderId = senderId' }) <$> f (_senderId invitation')


  receiverId :: Lens' a ((Maybe AccountId))
  receiverId = invitation . go
    where
      go :: Lens' Invitation ((Maybe AccountId))
      go f invitation' = (\receiverId' -> invitation' { _receiverId = receiverId' }) <$> f (_receiverId invitation')


instance HasInvitation Invitation where
  invitation = id

class HasInvitationInfo a where
  invitationInfo :: Lens' a InvitationInfo

  infoFamily :: Lens' a FamilyName
  infoFamily = invitationInfo . go
    where
      go :: Lens' InvitationInfo FamilyName
      go f invitationInfo' = (\infoFamily' -> invitationInfo' { _infoFamily = infoFamily' }) <$> f (_infoFamily invitationInfo')


  infoSendingDevice :: Lens' a Text
  infoSendingDevice = invitationInfo . go
    where
      go :: Lens' InvitationInfo Text
      go f invitationInfo' = (\infoSendingDevice' -> invitationInfo' { _infoSendingDevice = infoSendingDevice' }) <$> f (_infoSendingDevice invitationInfo')


  infoSendingUser :: Lens' a ((Maybe Text))
  infoSendingUser = invitationInfo . go
    where
      go :: Lens' InvitationInfo ((Maybe Text))
      go f invitationInfo' = (\infoSendingUser' -> invitationInfo' { _infoSendingUser = infoSendingUser' }) <$> f (_infoSendingUser invitationInfo')


instance HasInvitationInfo InvitationInfo where
  invitationInfo = id

