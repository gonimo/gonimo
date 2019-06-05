{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  BackendRoute_Api   :: BackendRoute (R ApiRoute)

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Parent :: FrontendRoute ()
  FrontendRoute_Baby :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
  --

-- | WebSocket endpoints serving a particular version of the API.
data ApiRoute :: * -> * where
  ApiRoute_V1 :: ApiRoute ()

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Api -> PathSegment "api" $
        pathComponentEncoder $ \case
          ApiRoute_V1-> PathSegment "v1" $ unitEncoder mempty
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Parent -> PathSegment "parent" $ unitEncoder mempty
      FrontendRoute_Baby -> PathSegment "baby" $ unitEncoder mempty


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ApiRoute
  ]
