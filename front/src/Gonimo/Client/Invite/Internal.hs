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
import Control.Monad.Fix (MonadFix)

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configSelectedFamily :: Dynamic t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateInvitation :: Event t ()
           }

data Invite t
  = Invite { _invitation :: Dynamic t (Maybe (InvitationId, Db.Invitation))
           , _request :: Event t [ API.ServerRequest ]
           }

makeLenses ''Config
makeLenses ''Invite

invite :: forall t m. (MonadHold t m, MonadFix m) => Reflex t => Config t -> m (Invite t)
invite config = mdo
  let
    currentSelected = current (config^.configSelectedFamily)
    createOnAuth = push (\() -> do
                            cInv <- sample $ current inv
                            case cInv of
                              Nothing -> pure $ Just ()
                              Just _ -> pure Nothing
                        ) (config^.configAuthenticated)

    createInvReq
      = (:[]) . API.ReqCreateInvitation
        <$> leftmost [ updated (config^.configSelectedFamily)
                     , tag currentSelected (config^.configCreateInvitation)
                     , tag currentSelected createOnAuth
                     ]
    invEv = push (\res -> case res of
                     API.ResCreatedInvitation invTuple -> pure $ Just invTuple
                     _ -> pure Nothing
                 ) (config^.configResponse)
  inv <- holdDyn Nothing $ Just <$> invEv
  pure Invite { _invitation = inv
              , _request = createInvReq
              }

getBaseLink :: MonadIO m => m Text
getBaseLink = do
  window  <- DOM.currentWindowUnchecked
  location <- Window.getLocationUnsafe window
  protocol <- Location.getProtocol location
  host <- Location.getHost location
  pathName <- Location.getPathname location
  pure $ protocol <> "//" <> host <> pathName

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
