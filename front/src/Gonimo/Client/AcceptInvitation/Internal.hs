{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.AcceptInvitation.Internal where

import Reflex.Dom.Core
import Control.Monad
import Control.Lens
import Data.Text (Text)
import Gonimo.Types (Secret)
import qualified Gonimo.SocketAPI as API
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import GHCJS.DOM.Types (toJSVal) 
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM as DOM
import Data.Maybe (maybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Network.HTTP.Types (urlDecode)
import Gonimo.SocketAPI.Types (InvitationReply)
import Gonimo.Client.Subscriber (SubscriptionsDyn)

import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.Auth as Auth
import           GHCJS.DOM.Types (MonadJSM, liftJSM)
import qualified Data.Set                                as Set

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configAuthenticated :: Event t ()
           }

data AcceptInvitation t
  = AcceptInvitation { _request :: Event t [ API.ServerRequest ]
                     , _subscriptions :: SubscriptionsDyn t
                     }

makeLenses ''Config
makeLenses ''AcceptInvitation


fromApp :: Reflex t => App.Config t -> Config t
fromApp c = Config { _configResponse = c^.App.server.webSocket_recv
                   , _configAuthenticated = c^.App.auth^.Auth.authenticated
                   }

getInvitationSecret :: forall m. (MonadPlus m, MonadJSM m) => m Secret
getInvitationSecret = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    queryString <- Location.getSearch location
    let secretString =
          let
            (_, startSecret) = T.drop 1 <$> T.breakOn "=" queryString
          in
            T.takeWhile (/='&') startSecret
    guard $ not (T.null secretString)
    let mDecoded = Aeson.decodeStrict . urlDecode True . T.encodeUtf8 $ secretString
    maybe mzero pure $ mDecoded

clearInvitationFromURL :: forall m. (MonadJSM m) => m ()
clearInvitationFromURL = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    history <- Window.getHistory window
    href <- Location.getHref location
    emptyJSVal <- liftJSM $ toJSVal T.empty
    History.pushState history emptyJSVal ("gonimo" :: Text) (Just $ T.takeWhile (/='?') href)

makeAnswerInvitation :: forall t. (Reflex t) => Secret -> Event t InvitationReply -> Event t [API.ServerRequest]
makeAnswerInvitation secret reply
  = (:[]) . API.ReqAnswerInvitation secret  <$> reply

emptyAcceptInvitation :: Reflex t => AcceptInvitation t
emptyAcceptInvitation = AcceptInvitation never (constDyn Set.empty)
