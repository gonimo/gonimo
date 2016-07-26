{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Gonimo.Server.EmailInvitation (
  makeInvitationEmail) where

import           Data.Text                (Text)
#ifndef DEVELOPMENT
import           NeatInterpolation
#endif 
import           Network.Mail.Mime        (Address (..), Mail, simpleMail')
import           Web.HttpApiData
import qualified Data.Text.Lazy           as TL
import           Data.Monoid

import           Gonimo.Server.DbEntities hiding (familyName)
import           Gonimo.Server.Types


#ifdef DEVELOPMENT
invitationText :: Invitation -> FamilyName -> Text
invitationText _inv _n = "New invitation for family "
    <> _n
    <> ":\nhttp://localhost:8081/index.html?acceptInvitation="
    <> secret
    <> "\n"
  where
    secret = toUrlPiece $ invitationSecret _inv
#else
invitationText :: Invitation -> FamilyName -> Text
invitationText _inv _n =
  [text|
    Dear User of gonimo.com!

    You got invited to join gonimo family "$_n"!
    Just click on the link below and you are all set for the best baby monitoring on the planet!

    https://test.gonimo.com/index.html?acceptInvitation=$secret

    Sincerely yours,

    Gonimo
  |]
  where
    secret = toUrlPiece $ invitationSecret _inv
#endif

makeInvitationEmail :: Invitation -> EmailAddress -> FamilyName -> Mail
makeInvitationEmail inv addr name = simpleMail' receiver sender "You got invited to a family on gonimo.com" (TL.fromStrict textContent)
  where
    textContent = invitationText inv name
    receiver = Address Nothing addr
    sender = Address Nothing "noreply@gonimo.com"
