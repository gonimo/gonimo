{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.AcceptInvitation.Internal where

import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId, InvitationId)
import Gonimo.Types (Secret)
import qualified Gonimo.Db.Entities as Db
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import qualified GHCJS.DOM.JSFFI.Generated.Location as Location
import GHCJS.DOM.Types (ToJSVal, toJSVal, FromJSVal, fromJSVal, JSVal)
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified GHCJS.DOM as DOM
import Control.Monad.Trans.Maybe
import Data.Maybe (maybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (urlDecode)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import Gonimo.SocketAPI.Types (InvitationReply)

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthenticated :: Event t ()
           }

data AcceptInvitation t
  = AcceptInvitation { _request :: Event t [ API.ServerRequest ]
                     }

makeLenses ''Config
makeLenses ''AcceptInvitation

getInvitationSecret :: forall m. (MonadPlus m, MonadIO m) => m Secret
getInvitationSecret = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocationUnsafe window
    queryString <- Location.getSearch location
    let secretString =
          let
            (_, startSecret) = T.drop 1 <$> T.breakOn "=" queryString
          in
            T.takeWhile (/='&') startSecret
    guard $ not (T.null secretString)
    let mDecoded = Aeson.decodeStrict . urlDecode True . T.encodeUtf8 $ secretString
    maybe mzero pure $ mDecoded

clearInvitationFromURL :: forall m. (MonadIO m) => m ()
clearInvitationFromURL = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocationUnsafe window
    Location.setSearch location ("" :: Text)

makeClaimInvitation :: forall t. (Reflex t) => Config t -> Secret -> Event t [API.ServerRequest]
makeClaimInvitation config secret
  = const [ API.ReqClaimInvitation secret ] <$> config^.configAuthenticated

makeAnswerInvitation :: forall t. (Reflex t) => Secret -> Event t InvitationReply -> Event t [API.ServerRequest]
makeAnswerInvitation secret reply
  = (:[]) . API.ReqAnswerInvitation secret  <$> reply

emptyAcceptInvitation :: Reflex t => AcceptInvitation t
emptyAcceptInvitation = AcceptInvitation never
