module Gonimo.WebAPI where

import Data.Text (Text)

import Servant.API
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON)
import Data.Proxy

type EmailAddress = Text
type FamilyId = Int
type Sender = Text
data Invitation = EmailInvitation EmailAddress deriving Generic


instance FromJSON Invitation


                  
type GonimoAPI = "families" :> FamiliesAPI 

type FamiliesAPI = -- Post '[JSON] FamilyId
  -- :<|> Capture "familyId" FamilyId :> FamilyAPI
                   Capture "familyId" FamilyId :> FamilyAPI

type FamilyAPI = "invitations" :> InvitationAPI
--                 :<|>  "senders" :> Get '[JSON] [Sender]
--                 :<|>  "senders" :> ReqBody '[JSON] Sender :> Post '[] ()

type InvitationAPI = -- Get '[JSON] [Invitation]
  -- :<|> ReqBody '[JSON] Invitation :> Post '[] ()
   ReqBody '[JSON] Invitation :> Post '[] ()



gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy
       
