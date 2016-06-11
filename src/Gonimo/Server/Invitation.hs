{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Gonimo.Server.Invitation where

import Gonimo.Server.Effects
import NeatInterpolation 
import Data.Text (Text)
import Network.Mail.Mime (Address(..))
import Network.Mail.Mime (Mail)
import Control.Monad.Freer (Eff)
import Network.Mail.Mime (simpleMail')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


