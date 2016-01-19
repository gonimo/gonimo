{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Gonimo.Server.EmailInvitation where

import Gonimo.Server.Effects
import NeatInterpolation 
import Data.Text (Text)
import Network.Mail.Mime (Address(..))
import Network.Mail.Mime (Mail)
import Control.Monad.Freer (Eff)
import Network.Mail.Mime (simpleMail')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL



invitationText :: TL.Text
invitationText = TL.pack -- Current version (not on nixos supports text quasi quoter) - Fix that, once we upgrade.
  [string|
    Dear User of gonimo.com!

    Just click on the link below and you are all set for the best baby monitoring on the planet!

    https://gonimo.com?someKey.....
  |]

makeInvitation :: Address -> Mail
makeInvitation addr = simpleMail' addr (Address Nothing "noreply@gonimo.com") "You got invited to a family on gonimo.com" invitationText

sendInvitation :: ServerConstraint r => Address -> Eff r ()
sendInvitation = sendEmail . makeInvitation
