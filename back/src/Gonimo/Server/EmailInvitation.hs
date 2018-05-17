{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Gonimo.Server.EmailInvitation (
  makeInvitationEmail) where

import           Data.Text                ()
import           Data.Text                as T
import           Data.Text.Encoding       as T
#ifndef DEVELOPMENT
import           NeatInterpolation
#endif
import           Data.Monoid
import           Network.Mail.Mime        (Address (..), Mail, simpleMailInMemory)
import qualified Data.Text.Lazy           as TL
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types (urlEncode)

import           Gonimo.Types
import qualified Data.Aeson as Aeson


type EmailAddress = Text

#ifdef DEVELOPMENT
invitationMail :: Text -> Text -> Text -> (Text, Text)
invitationMail invURL famName _ = (textStuff, "")
  where
    textStuff =
      "New invitation for family "
      <> famName
      <> ":\n"
      <> invURL
      <> "\n"

#else
invitationMail :: Text -> Text -> Text -> (Text, Text)
invitationMail invURL famName invDevice =
  ( [text|
      Welcome to GONIMO!

      You got invited by "$invDevice" to join "$famName"!

      Click the following link for accepting the invitation:

      $invURL

      Gonimo is a free, secure and reliable baby monitor, only using your
      browser (Chrome works best) to stream video and/or audio. Working
      out of the box without registration or login. Just take your laptop or
      Android phone or tablet and use it to take care of your children.

      Soon available on Google Play!

    |]
  , [text|
      <!DOCTYPE html>
      <html>
        <head>
          <meta http-equiv="content-type" content="text/html; charset=utf-8">
          <meta name="viewport" content="width=device-width">
          <title>Invitation to Family "$famName"</title>
        </head>
        <body text="#000000" bgcolor="#eee" style="text-align:center;">
          <meta charset="utf-8">
          <div style="background:#fff; width:90%;padding:2% 0%;margin:auto;">
            <div style="text-align:center;">
              <h1>Welcome to GONIMO</h1>
              <img src="https://gonimo.com/img/ico/usp/gonimo-02-342ed36bf433629ee761649b5ec51da2.svg" alt="Gonimo invitation" height="200px">
              <h2>You got invited by "$invDevice" to join "$famName"!</h2>
              <p style="padding:2%;">Click the button to accept the invitation:</p>
            </div>
            <div style="text-align:center;margin: 10pt auto;"> <a role="button" style="font-size: 200%;padding: 2% 10%;border:3pt solid #dec0ca;background:#dec0ca;color:white;text-decoration:none;display:inline-block;" href="$invURL">Load Invitation </a> </div>
            <p style="padding:2% 0%;text-align:center;"> Gonimo is a free,
            secure and reliable baby monitor, only using your browser (Chrome
            works best) to stream video and/or audio. Working out of the box
            without registration or login. Just take your laptop or Android
            phone or tablet and use it to take care of your children. </p>
            <p style="padding:2%;text-align:center;"> Soon available on Google
            Play! </p>
          </div>
        </body>
      </html>
    |]
  )

#endif

makeInvitationEmail :: Text -> Secret -> EmailAddress -> Text -> Text -> Mail
makeInvitationEmail baseURL secret' addr famName invitingDevice = simpleMailInMemory receiver sender "Gonimo Family Invitation" (TL.fromStrict asText) (TL.fromStrict asHTML) []
  where
    invURL =  baseURL <> "?acceptInvitation=" <> secret
    (asText, asHTML) = invitationMail invURL famName invitingDevice
    receiver = Address Nothing addr
    sender = Address Nothing "hello@gonimo.com"
    secret = T.decodeUtf8 . urlEncode True . encodeStrict $ secret'
    encodeStrict = BL.toStrict . Aeson.encode
    -- sender = if T.isSuffixOf "gonimo.com" addr
    --   then Address Nothing "hello@baby.gonimo.com" -- So we can send emails to ourself. (noreply@gonimo.com gets blocked by easyname)
    --   else Address Nothing "hello@gonimo.com"
