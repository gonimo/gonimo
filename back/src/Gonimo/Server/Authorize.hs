{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Server.Authorize
Description : Short description
Copyright   : (c) Robert Klotzner, 2017
Authorizes incoming requests.
-}
module Gonimo.Server.Authorize ( -- * Types & Classes
                                 Config (..)
                               , HasConfig (..)
                               , Authorize (..)
                               , HasAuthorize (..)
                               -- * Creation
                               , make

                               ) where


import           Reflex

import           Gonimo.Prelude
import           Gonimo.Server.Authorize.Internal
import           Gonimo.Server.Clients.Internal   (HasClients (..))
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Model



-- | Authorize requests
make :: forall t. Reflex t => Config t -> Authorize t
make conf =
  let
    (_onForbidden, _onAuthorized) = fanEither . fmap doAuthorize $ onAuthRequest

    onAuthRequest :: Event t AuthRequest
    onAuthRequest = pushAlways mkAuthRequest (conf^.onAuthorize)

    doAuthorize :: AuthRequest -> Either (DeviceId, ToClient) (DeviceId, FromClient)
    doAuthorize auth = fromMaybe (Right $ toAuthorized auth)
                       $ Left <$> mkForbidden auth

    mkForbidden :: AuthRequest -> Maybe (DeviceId, ToClient)
    mkForbidden = sequence . (senderId &&& denyClient)

    toAuthorized :: AuthRequest -> (DeviceId, FromClient)
    toAuthorized = senderId &&& msg

    mkAuthRequest (devId, msg) = do
      model' <- sample $ conf^.cache
      clients'' <- sample $ conf^.statuses
      pure $ AuthRequest model' clients'' devId msg
  in
    Authorize {..}
