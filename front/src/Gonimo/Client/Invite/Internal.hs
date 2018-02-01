{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.Invite.Internal where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix    (MonadFix)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Default         (Default (..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T
import qualified GHCJS.DOM            as DOM
import qualified GHCJS.DOM.Document   as Document
import qualified GHCJS.DOM.Location   as Location
import           GHCJS.DOM.Types      (MonadJSM)
import           Network.HTTP.Types   (urlEncode)
import           Reflex.Dom.Core
import           Data.Maybe           (fromMaybe)

import           Gonimo.SocketAPI.Types   (FamilyId, InvitationId)
import qualified Gonimo.SocketAPI     as API
import qualified Gonimo.SocketAPI.Types     as API
import Gonimo.Client.Config (httpProtocol, gonimoFrontHost, gonimoFrontPath)

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configSelectedFamily :: Dynamic t FamilyId
           , _configAuthenticated :: Event t ()
           , _configCreateInvitation :: Event t ()
           }

data Invite t
  = Invite { _invitation :: Dynamic t (Maybe (InvitationId, API.Invitation))
           , _request :: Event t [ API.ServerRequest ]
           , _uiGoBack :: Event t ()
           , _uiDone :: Event t()
           }

data InvitationSent
  = SentWhatsApp
  | SentTelegram
  | SentCopy
  | SentRefresh
  | SentEmail

invite :: forall t m. (MonadHold t m, MonadFix m, Reflex t) => Config t -> m (Invite t)
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
              , _uiGoBack = never
              , _uiDone = never
              }

getBaseLink :: (MonadJSM m) => m Text
getBaseLink = do
  -- Don't use own url in general, you'll get file:/// uris in the app version:
  doc  <- DOM.currentDocumentUnchecked
  location <- Document.getLocationUnsafe doc
  -- protocol <- Location.getProtocol location
  ownHost <- Location.getHost location
  let
    protocol = httpProtocol
    host = fromMaybe ownHost gonimoFrontHost
    pathName = gonimoFrontPath
  pure $ protocol <> host <> pathName

makeInvitationLink :: Text -> API.Invitation -> Text
makeInvitationLink baseURL inv =
  let
    encodedSecret = T.decodeUtf8 .  urlEncode True . BL.toStrict . Aeson.encode . API.invitationSecret $ inv
  in
    baseURL <> "?" <> invitationQueryParam <> "=" <> encodedSecret

-- Not used currently : 
-- encodeURIComponent :: (ToJSVal i, FromJSVal o, MonadIO m) => i -> MaybeT m o
-- encodeURIComponent val = do
--   jsVal <- liftIO $ toJSVal val
--   let jsOut = jsEncodeURIComponent jsVal
--   MaybeT . liftIO $ fromJSVal jsOut

-- foreign import javascript unsafe
--   "encodeURIComponent $1"
--   jsEncodeURIComponent :: JSVal -> JSVal


instance Reflex t => Default (Invite t) where
  def = Invite { _invitation = constDyn Nothing
               , _request = never
               , _uiGoBack = never
               , _uiDone = never
               }

inviteSwitchPromptlyDyn :: Reflex t => Dynamic t (Invite t) -> Invite t
inviteSwitchPromptlyDyn dynInvite
  = Invite { _invitation = join (_invitation <$> dynInvite)
           , _request = switchPromptlyDyn (_request <$> dynInvite)
           , _uiGoBack = switchPromptlyDyn (_uiGoBack <$> dynInvite)
           , _uiDone = switchPromptlyDyn (_uiDone <$> dynInvite)
           }


-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configSelectedFamily :: Lens' (Config t) (Dynamic t FamilyId)
configSelectedFamily f config' = (\configSelectedFamily' -> config' { _configSelectedFamily = configSelectedFamily' }) <$> f (_configSelectedFamily config')

configAuthenticated :: Lens' (Config t) (Event t ())
configAuthenticated f config' = (\configAuthenticated' -> config' { _configAuthenticated = configAuthenticated' }) <$> f (_configAuthenticated config')

configCreateInvitation :: Lens' (Config t) (Event t ())
configCreateInvitation f config' = (\configCreateInvitation' -> config' { _configCreateInvitation = configCreateInvitation' }) <$> f (_configCreateInvitation config')


-- Lenses for Invite t:

invitation :: Lens' (Invite t) (Dynamic t (Maybe (InvitationId, API.Invitation)))
invitation f invite' = (\invitation' -> invite' { _invitation = invitation' }) <$> f (_invitation invite')

request :: Lens' (Invite t) (Event t [ API.ServerRequest ])
request f invite' = (\request' -> invite' { _request = request' }) <$> f (_request invite')

uiGoBack :: Lens' (Invite t) (Event t ())
uiGoBack f invite' = (\uiGoBack' -> invite' { _uiGoBack = uiGoBack' }) <$> f (_uiGoBack invite')

uiDone :: Lens' (Invite t) (Event t ())
uiDone f invite' = (\uiDone' -> invite' { _uiDone = uiDone' }) <$> f (_uiDone invite')


