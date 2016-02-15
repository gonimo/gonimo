{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gonimo.WebAPI.Docs where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Servant.API
import Servant.Docs
import Servant.Server
import Data.Text (Text)
import Data.ByteString (ByteString)

import Gonimo.WebAPI


-- Captures:
       
instance ToCapture (Capture "familyId" FamilyId) where
  toCapture _ =
    DocCapture "familyId"                                
    "(FamilyId) The id of the family you want to access."

instance ToCapture (Capture "invitationId" InvitationId) where
  toCapture _ =
    DocCapture "invitationId"                                
    "(InvitationId) The id of the invitation you want to access."
    
instance ToCapture (Capture "invitationSecret" InvitationSecret) where
  toCapture _ =
    DocCapture "invitationSecret"                                
    "(InvitationSecret) You need this secret in order to retrieve an invitation, if you are not already a family member."

-- Samples: 

instance ToSample AuthToken AuthToken where
  toSample _ = Nothing

instance ToSample FamilyId FamilyId where
  toSample _ = Just 12

instance ToSample [FamilyId] [FamilyId] where
  toSample _ = Just [1,2,23]

instance ToSample [(InvitationId, Invitation)] [(InvitationId, Invitation)] where
  toSample _ = Nothing

instance ToSample (InvitationId, Invitation) (InvitationId, Invitation) where
  toSample _ = Nothing

instance ToSample Sender Sender where
  toSample _ = Nothing

instance ToSample [Sender] [Sender] where
  toSample _ = Nothing

instance ToSample () () where
  toSample _ = Just ()

instance ToSample (Maybe Credentials) (Maybe Credentials) where
  toSample _ = Nothing

instance ToSample Credentials Credentials where
  toSample _ = Nothing

instance ToSample InvitationDelivery InvitationDelivery where
  toSample _ = Nothing

instance ToSample Invitation Invitation where
  toSample _ = Nothing

apiDocs :: API
apiDocs = docs gonimoAPI

          
