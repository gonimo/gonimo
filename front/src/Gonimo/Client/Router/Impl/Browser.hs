{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Router.Impl.Browser
Description : Routing for Gonimo.
Copyright   : (c) Robert Klotzner, 2018

Implementation for the Router interface that is based on the browser's history.
-}
module Gonimo.Client.Router.Impl.Browser ( module Gonimo.Client.Router
                                         , make
                                         ) where

import           Reflex.Dom.Contrib.Router
import           Network.URI            (uriPath)
import           Reflex.Dom.Core
import           System.FilePath (splitPath)

import           Gonimo.Client.Prelude
import           Gonimo.Client.Router


make :: (MonadWidget t m , HasConfig c)
     => c t -> m (Router t)
make conf = do
    _route <- route' encode decode (conf ^. onSetRoute)
    performEvent_ $ goBack <$ conf ^. onGoBack

    -- histPos0 <- getHistoryPosition
    -- newHistPos <- performEvent $ getHistoryPosition <$ updated _route
    -- _historyPosition <- holdDyn histPos0 newHistPos

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
    RouteBaby         -> "/baby"
    RouteParent       -> "/parent"

-- This function is total, because every unknown route will be routed to 'RouteHome'.
parseRoute :: Path -> Route
parseRoute p' =
  let p = case (drop 2 . splitPath) p' of
            [x] -> x
            _   -> p'
  in
    case p of
      "baby"         -> RouteBaby
      "parent"       -> RouteParent
      _              -> RouteHome
