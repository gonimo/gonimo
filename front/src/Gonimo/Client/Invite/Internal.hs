{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Invite.Internal where

import           Control.Lens
import           Control.Monad

import           Control.Monad.Fix           (MonadFix)
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BL
import           Data.Default                (Default (..))
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle
import           Network.HTTP.Types          (urlEncode)
import           Reflex.Dom.Core

import qualified Gonimo.Client.Environment   as Env
import qualified Gonimo.SocketAPI            as API
import           Gonimo.SocketAPI.Types      (FamilyId, InvitationId)
import qualified Gonimo.SocketAPI.Types      as API

invitationQueryParam :: Text
invitationQueryParam = "acceptInvitation"

data Config t
  = Config { _configResponse         :: Event t API.ServerResponse
           , _configSelectedFamily   :: Dynamic t FamilyId
           , _configAuthenticated    :: Event t ()
           , _configCreateInvitation :: Event t ()
           }

data Invite t
  = Invite { _invitation :: Dynamic t (Maybe (InvitationId, API.Invitation))
           , _request    :: Event t [ API.ServerRequest ]
           , _uiGoBack   :: Event t ()
           , _uiDone     :: Event t()
           }

data InvitationSent
  = SentWhatsApp
  | SentTelegram
  | SentCopy
  | SentRefresh
  | SentEmail
  | SentShare


type HasModel model = Env.HasEnvironment model

invite :: forall model t m. (MonadHold t m, MonadFix m, Reflex t, HasModel model)
  => model t -> Config t -> m (Invite t)
invite model config = mdo
  let
    currentSelected = current (config^.configSelectedFamily)
    createOnAuth = push (\() -> do
                            cInv <- sample $ current inv
                            case cInv of
                              Nothing -> pure $ Just ()
                              Just _  -> pure Nothing
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

getBaseLink :: HasModel model => model t -> Text
getBaseLink model = model ^. Env.httpProtocol <> model ^. Env.frontendHost <> model ^. Env.frontendPath

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

-- Browser capabilities
shareLink :: (MonadJSM m, MonadPlus m, MonadJSM m') => m (Text -> m' ())
shareLink = do
  nav    <- liftJSM $ jsg ("navigator" :: Text)
  win    <- liftJSM $ jsg ("window"    :: Text)
  mNativeShare      <- liftJSM $ maybeNullOrUndefined =<< nav ! ("share"        :: Text)
  mAndroidShare     <- liftJSM $ maybeNullOrUndefined =<< win ! ("nativeHost" :: Text)
  case (mNativeShare, mAndroidShare) of
    (Just _share, _) ->
      let shareFunc linkUrl = void $ liftJSM $ do
            url <- obj
            (url <# ("url" :: Text)) linkUrl
            nav ^. js1 ("share" :: Text) url
       in pure shareFunc
    (_, Just share) ->
      let shareFunc linkUrl = void $ liftJSM $
            share ^. js1 ("share" :: Text) linkUrl
       in pure shareFunc
    _ -> mzero


