{-|
Module      : Gonimo.Client.Auth.API
Description : User facing Auth API
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Auth.API where



import           Gonimo.Client.Prelude
import qualified Gonimo.SocketAPI.Types as API



data Auth t
  = Auth { -- | Our Authentication data, if available.
           _authData :: Dynamic t (Maybe API.AuthData)

           -- | Authenticate sucessful. Connection is fully established.
           --
           --   You should wait for this event before sending server requests that require authentication.
         , _onAuthenticated :: Event t () -- TODO: Now redundant, because of isOnline.

          -- | True if we are currently online (have a connection and are authenticated).
         , _isOnline :: Dynamic t Bool
         }


-- Auto generated lenses:

class HasAuth a where
  auth :: Lens' (a t) (Auth t)

  authData :: Lens' (a t) (Dynamic t (Maybe API.AuthData))
  authData = auth . go
    where
      go :: Lens' (Auth t) (Dynamic t (Maybe API.AuthData))
      go f auth' = (\authData' -> auth' { _authData = authData' }) <$> f (_authData auth')


  onAuthenticated :: Lens' (a t) (Event t ())
  onAuthenticated = auth . go
    where
      go :: Lens' (Auth t) (Event t ())
      go f auth' = (\onAuthenticated' -> auth' { _onAuthenticated = onAuthenticated' }) <$> f (_onAuthenticated auth')


  isOnline :: Lens' (a t) (Dynamic t Bool)
  isOnline = auth . go
    where
      go :: Lens' (Auth t) (Dynamic t Bool)
      go f auth' = (\isOnline' -> auth' { _isOnline = isOnline' }) <$> f (_isOnline auth')


instance HasAuth Auth where
  auth = id

