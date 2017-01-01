{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.SocketAPI where

import           Data.Text                       (Text)
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions,
                                        genericToEncoding)
import qualified Gonimo.SocketAPI.Types as Client
import GHC.Generics (Generic)
import Gonimo.Server.Error (ServerError)
import Gonimo.Db.Entities (FamilyId, InvitationId, Invitation)
import Gonimo.Types (AuthToken)

type MessageId = Int
data ServerRequest
  = ReqMakeDevice !(Maybe Text)
  | ReqAuthenticate !AuthToken
  | ReqCreateFamily
  | ReqCreateInvitation !FamilyId
  | ReqSetSubscriptions !([ServerRequest])
  deriving (Generic, Ord, Eq)

data ServerResponse = ResMadeDevice !Client.AuthData
  | ResAuthenticated
  | ResCreatedFamily !FamilyId
  | ResCreatedInvitation !(InvitationId, Invitation)
  | ResError !ServerRequest !ServerError
  deriving (Generic)

instance FromJSON ServerRequest
instance ToJSON ServerRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerResponse
instance ToJSON ServerResponse where
  toEncoding = genericToEncoding defaultOptions
