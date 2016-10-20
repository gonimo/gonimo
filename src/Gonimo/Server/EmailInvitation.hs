{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Gonimo.Server.EmailInvitation (
  makeInvitationEmail) where

import           Data.Text                ()
import           Data.Text                as T
#ifndef DEVELOPMENT
import           NeatInterpolation
#else
import           Data.Monoid
import           Network.HTTP.Types (urlEncode, urlDecode)
#endif
import           Network.Mail.Mime        (Address (..), Mail, simpleMail')
import           Web.HttpApiData
import qualified Data.Text.Lazy           as TL

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
    secret = T.decodeUtf8 . urlEncode False . T.encodeUtf8 . toUrlPiece $ invitationSecret _inv
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
    sender = if T.isSuffixOf "gonimo.com" addr
      then Address Nothing "noreply@baby.gonimo.com" -- So we can send emails to ourself. (noreply@gonimo.com gets blocked by easyname)
      else Address Nothing "noreply@gonimo.com"
