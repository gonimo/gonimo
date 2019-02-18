{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Environment
Description : Get configurations from the environment.
Copyright   : (c) Robert Klotzner, 2018


Retrieved Obelisk configuration values..
-}
module Gonimo.Client.Environment (
                               -- * Types
                               Environment(..)
                             , HasEnvironment(..)
                               -- * Creation
                             , make
                             ) where


import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.Document          as Document
import qualified GHCJS.DOM.Location          as Location
import           GHCJS.DOM.Types             (MonadJSM, liftJSM)
import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSVal,
                                              fromJSVal, js, jsg)
import           Text.URI                    (Authority (authHost),
                                              URI (uriAuthority, uriScheme),
                                              mkScheme, mkURI, unRText)

import           Gonimo.Client.Prelude
import           Obelisk.ExecutableConfig    (get)


data Environment t -- Dummy `t` for compatibility with other models.
  = Environment { -- | On what server is the frontend running.
                  --
                  --   Example value: "app.gonimo.com"
                  --
                  --   This setting is typically derived from backendHost or if
                  --   applicable read from the browser via 'Location.getHost'.
                  _frontendHost       :: Text

                  -- | Where is our backend located?
                  --
                  --   Example value: "b00.gonimo.com"
                , _backendHost        :: Text

                  -- | What WebSocket URL to use for backend connections.
                  --
                  --   Gets derived from 'backendHost'.
                  --
                  --   Example value: "wss://b00.gonimo.com"
                , _backendWSURL       :: Text

                  -- | Path component for retrieving the frontend.
                  --
                  --   Examples: "/" or "/index.html"
                , _frontendPath       :: Text

                  -- | HTTP protocol in use.
                  --
                  --   It is either "http://" or "https://"
                , _httpProtocol       :: Text

                  -- | Where is our TURN server?
                  --
                  --   Example value: "turn:gonimo.com:3478"
                , _turnConnection     :: Text

                  -- | The user for the TURN server connection.
                , _turnUser           :: Text

                  -- | What password to use for the connection to the TURN server?
                , _turnPassword       :: Text

                  -- | How do we authenticate to the TURN server?
                  --
                  --   Example value: "password"
                , _turnCredentialType :: Text
                }

-- | Create an Environment.
--
--   The environment is loaded from the JavaScript values set in env.js.
make ::  MonadJSM m => m (Environment t)
make = liftJSM $ do
  uri <- mkURI =<< getCfg "config/common/route"
  let
    scheme = fromMaybe (error "config/common/route: scheme missing!") $ uriScheme uri
    secure = mkScheme "https" == uriScheme uri
    authority = either (error "common/route is invalid!") id $ uriAuthority uri
    _backendHost = unRText $ authHost authority
  _turnConnection <- getCfg "config/frontend/turn/connection"
  _turnUser <- getCfg "config/frontend/turn/user"
  _turnPassword <- getCfg "config/frontend/turn/password"
  _turnCredentialType <- getCfg "config/frontend/turn/credential-type"

  _frontendHost <- makeFrontendHostFromBackend _backendHost
  liftIO . T.putStrLn $ "gonimo, backend host: " <> _backendHost

  let _httpProtocol = unRText scheme
  let _frontendPath = "/"

  let wsPrefix      = if secure then "wss://" else "ws://"
  let _backendWSURL = wsPrefix <> _backendHost

  liftIO . T.putStrLn $ "gonimo, backend ws url: " <> _backendWSURL

  pure $ Environment {..}


-- | Calculate the frontend host from the backend url.
--
--   For localhost, this falls back to Location.getHost.
makeFrontendHostFromBackend :: MonadJSM m => Text -> m Text
makeFrontendHostFromBackend backend
  = if T.isInfixOf "localhost" backend
    then do
      doc  <- DOM.currentDocumentUnchecked
      location <- Document.getLocationUnsafe doc
      Location.getHost location
    else
      pure $ "app" <> T.dropWhile (/= '.') backend


getCfg :: MonadIO m => Text -> m Text
getCfg k =
  T.strip . fromMaybe (error $ "Fatal: Config '" <> T.unpack k <> "' missing!") <$> liftIO (get k)

-- Auto generated lenses:


class HasEnvironment a where
  environment :: Lens' (a t) (Environment t)

  frontendHost :: Lens' (a t) Text
  frontendHost = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\frontendHost' -> environment' { _frontendHost = frontendHost' }) <$> f (_frontendHost environment')


  backendHost :: Lens' (a t) Text
  backendHost = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\backendHost' -> environment' { _backendHost = backendHost' }) <$> f (_backendHost environment')


  backendWSURL :: Lens' (a t) Text
  backendWSURL = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\backendWSURL' -> environment' { _backendWSURL = backendWSURL' }) <$> f (_backendWSURL environment')


  frontendPath :: Lens' (a t) Text
  frontendPath = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\frontendPath' -> environment' { _frontendPath = frontendPath' }) <$> f (_frontendPath environment')


  httpProtocol :: Lens' (a t) Text
  httpProtocol = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\httpProtocol' -> environment' { _httpProtocol = httpProtocol' }) <$> f (_httpProtocol environment')


  turnConnection :: Lens' (a t) Text
  turnConnection = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\turnConnection' -> environment' { _turnConnection = turnConnection' }) <$> f (_turnConnection environment')


  turnUser :: Lens' (a t) Text
  turnUser = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\turnUser' -> environment' { _turnUser = turnUser' }) <$> f (_turnUser environment')


  turnPassword :: Lens' (a t) Text
  turnPassword = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\turnPassword' -> environment' { _turnPassword = turnPassword' }) <$> f (_turnPassword environment')


  turnCredentialType :: Lens' (a t) Text
  turnCredentialType = environment . go
    where
      go :: Lens' (Environment t) Text
      go f environment' = (\turnCredentialType' -> environment' { _turnCredentialType = turnCredentialType' }) <$> f (_turnCredentialType environment')


instance HasEnvironment Environment where
  environment = id

