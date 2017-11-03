{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.AcceptInvitation.Internal where

import           Control.Lens
import           Control.Monad
import qualified Data.Aeson         as Aeson
import           Data.Maybe         (maybe)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM          as DOM
import qualified GHCJS.DOM.History  as History
import qualified GHCJS.DOM.Location as Location
import           GHCJS.DOM.Types    (toJSVal)
import           GHCJS.DOM.Types    (MonadJSM, liftJSM)
import qualified GHCJS.DOM.Window   as Window
import           Network.HTTP.Types (urlDecode)
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types  as App
import qualified Gonimo.Client.Auth       as Auth
import           Gonimo.Client.Server hiding (Config)
import           Gonimo.Client.Subscriber (SubscriptionsDyn)
import qualified Gonimo.SocketAPI         as API
import           Gonimo.SocketAPI.Types   (InvitationReply)
import           Gonimo.Types             (Secret)

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


fromApp :: Reflex t => App.Config t -> Config t
fromApp c = Config { _configResponse = c^.server.response
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

-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthenticated :: Lens' (Config t) (Event t ())
configAuthenticated f config' = (\configAuthenticated' -> config' { _configAuthenticated = configAuthenticated' }) <$> f (_configAuthenticated config')


-- Lenses for AcceptInvitation t:

request :: Lens' (AcceptInvitation t) (Event t [ API.ServerRequest ])
request f acceptInvitation' = (\request' -> acceptInvitation' { _request = request' }) <$> f (_request acceptInvitation')

subscriptions :: Lens' (AcceptInvitation t) (SubscriptionsDyn t)
subscriptions f acceptInvitation' = (\subscriptions' -> acceptInvitation' { _subscriptions = subscriptions' }) <$> f (_subscriptions acceptInvitation')


