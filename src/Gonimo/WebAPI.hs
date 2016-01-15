{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gonimo.WebAPI where

import Data.Text (Text)

import Servant.API

type EmailAddress = Text
type FamilyId = Int
type Sender = Text

data Invitation = EmailInvitation EmailAddress

type GonimoAPI = "families" :> FamiliesAPI 

type FamiliesAPI = Post '[JSON] FamilyId
                   :<|> Capture "familyId" FamilyId :> FamilyAPI

type FamilyAPI = "invitations" :> InvitationAPI
                 :<|>  "senders" :> Get '[JSON] [Sender]
                 :<|>  "senders" :> ReqBody '[JSON] Sender :> Post '[] ()

type InvitationAPI = Get '[JSON] [Invitation]
  :<|> ReqBody '[JSON] Invitation :> Post '[] ()



       
