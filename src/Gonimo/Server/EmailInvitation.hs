{-# LANGUAGE QuasiQuotes #-}
module Gonimo.Server.EmailInvitation (
  makeInvitationEmail) where

import Gonimo.Server.Effects
import NeatInterpolation
import Data.Text (Text)
import Network.Mail.Mime (Address(..))
import Network.Mail.Mime (Mail)
import Control.Monad.Freer (Eff)
import Network.Mail.Mime (simpleMail')
import Gonimo.Server.DbEntities hiding (familyName)
import Gonimo.Server.DbTypes
import Gonimo.Types

import qualified Data.Text.Lazy as TL



invitationText :: Invitation -> Text -> Text
invitationText inv familyName=
  [text|
    Dear User of gonimo.com!

    You got invited to join gonimo family "#{familyName}"!
    Just click on the link below and you are all set for the best baby monitoring on the planet!

    https://gonimo.com/acceptInvitation?#{secret inv}
  |]


makeInvitationEmail :: Invitation -> EmailAddress -> Text -> Mail
makeInvitationEmail inv addr familyName = simpleMail' receiver sender "You got invited to a family on gonimo.com" (TL.fromStrict textContent)
  where
    textContent = invitationText inv familyName
    receiver = Address Nothing addr
    sender = Address Nothing "noreply@gonimo.com"
