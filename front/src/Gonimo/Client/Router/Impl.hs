{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Router.Impl
Description : Routing for Gonimo.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Router.Impl ( module Gonimo.Client.Router
                                 , make
                                 ) where

import           Reflex.Dom.Contrib.Router
import           Network.URI            (uriPath)
import           Reflex.Dom.Core
import           Data.List (stripPrefix)

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



type Path = String

renderRoute :: Route -> Path
renderRoute r = "/index" <> ".html" <> -- This artifical split is necessary because of our current filename hashing. Otherwise the route gets replaced with index-hash.html.
  case r of
    RouteHome         -> ""
    RouteCreateFamily -> "/createFamily"
    RouteInvite       -> "/invite"
    RouteBaby         -> "/baby"
    RouteParent       -> "/parent"

-- This function is total, because every unknown route will be routed to 'RouteHome'.
parseRoute :: Path -> Route
parseRoute p' =
  let p = fromMaybe p' $ stripPrefix "/index.html" p'
  in
    case p of
      "/createFamily" -> RouteCreateFamily
      "/invite"       -> RouteInvite
      "/baby"         -> RouteBaby
      "/parent"       -> RouteParent
      _               -> RouteHome
