{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.Invite.Internal where

import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId, InvitationId)
import qualified Gonimo.Db.Entities as Db
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import qualified GHCJS.DOM.JSFFI.Generated.Location as Location
import GHCJS.DOM.Types (ToJSVal, toJSVal, FromJSVal, fromJSVal, JSVal)
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified GHCJS.DOM as DOM
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (urlEncode)

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configSelectedFamily :: Dynamic t FamilyId
           , _configCreateInvitation :: Event t ()
           }

data Invite t
  = Invite { _invitation :: Event t (InvitationId, Db.Invitation)
           , _request :: Event t [ API.ServerRequest ]
           }

makeLenses ''Config
makeLenses ''Invite

invite :: forall t. Reflex t => Config t -> Invite t
invite config =
  let
    createInvReq = push (\_ -> do
                            cFamily <- sample $ current (config^.configSelectedFamily)
                            pure $ Just . (:[]) . API.ReqCreateInvitation $ cFamily
                        ) (leftmost [ config^.configCreateInvitation
                                    , const () <$> updated (config^.configSelectedFamily)
                                    ]
                          )
    invEv = push (\res -> case res of
                     API.ResCreatedInvitation invTuple -> pure $ Just invTuple
                     _ -> pure Nothing
                 ) (config^.configResponse)
  in
    Invite { _invitation = invEv
           , _request = createInvReq
           }

getBaseLink :: MonadIO m => m Text
getBaseLink = do
  window  <- DOM.currentWindowUnchecked
  location <- Window.getLocationUnsafe window
  Location.getHref location

makeInvitationLink :: Text -> Db.Invitation -> Text
makeInvitationLink baseURL inv =
  let
    encodedSecret = T.decodeUtf8 .  urlEncode True . BL.toStrict . Aeson.encode . Db.invitationSecret $ inv
  in
    baseURL <> "?" <> invitationQueryParam <> "=" <> encodedSecret

encodeURIComponent :: (ToJSVal i, FromJSVal o, MonadIO m) => i -> MaybeT m o
encodeURIComponent val = do
  jsVal <- liftIO $ toJSVal val
  let jsOut = jsEncodeURIComponent jsVal
  MaybeT . liftIO $ fromJSVal jsOut

foreign import javascript unsafe
  "encodeURIComponent $1"
  jsEncodeURIComponent :: JSVal -> JSVal
