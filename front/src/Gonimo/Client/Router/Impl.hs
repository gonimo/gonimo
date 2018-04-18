{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Router.Impl
Description : Routing for Gonimo.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Router.Impl where

import           Data.ByteString           (ByteString)
import           Reflex.Dom.Contrib.Router
import           URI.ByteString            (uriPath)
import           Reflex.Dom.Core

import           Gonimo.Client.Prelude
import           Gonimo.Client.Router


make :: (MonadWidget t m , HasConfig c)
     => c t -> m (Router t)
make conf = do
    _route <- route' encode decode (conf ^. onSetRoute)
    performEvent_ $ goBack <$ conf ^. onGoBack
    pure $ Router {..}
  where
    encode :: URI -> Route -> URI
    encode uri r = uri { uriPath = renderRoute r }

    decode :: URI -> Route
    decode = parseRoute . uriPath



type Path = ByteString

renderRoute :: Route -> Path
renderRoute r =
  case r of
    RouteHome         -> "/"
    RouteCreateFamily -> "createFamily"
    RouteInvite       -> "invite"
    RouteBaby         -> "baby"
    RouteParent       -> "parent"

-- This function is total, because every unknown route will be routed to 'RouteHome'.
parseRoute :: Path -> Route
parseRoute p =
  case p of
    "createFamily" -> RouteCreateFamily
    "invite"       -> RouteInvite
    "baby"         -> RouteBaby
    "parent"       -> RouteParent
    _              -> RouteHome
