{-|
Module      : Gonimo.Client.Router
Description : Gonimo routes.
Copyright   : (c) Robert Klotzner, 2018
Routes within the app the user will be able to navigate.
-}
module Gonimo.Client.Router ( -- * Types
                              Config(..)
                            , HasConfig(..)
                            , Router(..)
                            , HasRouter(..)
                            , Route(..)
                            ) where


import Reflex


import           Gonimo.Client.Prelude


data Config t
  = Config { _onSetRoute :: Event t Route
           , _onGoBack :: Event t ()
           } deriving Generic


data Router t
  = Router { _route :: Dynamic t Route
           }



data Route = RouteHome
           | RouteBaby
           | RouteParent
           deriving (Eq, Show)


instance Reflex t => Default (Config t) where
  def = Config never never

instance Reflex t => Semigroup (Config t) where
  c1 <> c2 = Config { _onSetRoute = leftmost [ _onSetRoute c1
                                             , _onSetRoute c2
                                             ]
                    , _onGoBack = leftmost [ _onGoBack c1
                                           , _onGoBack c2
                                           ]
                    }

instance Reflex t => Monoid (Config t) where
  mempty = Config never never
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onSetRoute <$> ev)
      <*> doSwitch never (_onGoBack <$> ev)

-- Auto generated lenses:

class HasConfig a where
  config :: Lens' (a t) (Config t)

  onSetRoute :: Lens' (a t) (Event t Route)
  onSetRoute = config . go
    where
      go :: Lens' (Config t) (Event t Route)
      go f config' = (\onSetRoute' -> config' { _onSetRoute = onSetRoute' }) <$> f (_onSetRoute config')


  onGoBack :: Lens' (a t) (Event t ())
  onGoBack = config . go
    where
      go :: Lens' (Config t) (Event t ())
      go f config' = (\onGoBack' -> config' { _onGoBack = onGoBack' }) <$> f (_onGoBack config')


instance HasConfig Config where
  config = id



class HasRouter a where
  router :: Lens' (a t) (Router t)

  route :: Lens' (a t) (Dynamic t Route)
  route = router . go
    where
      go :: Lens' (Router t) (Dynamic t Route)
      go f router' = (\route' -> router' { _route = route' }) <$> f (_route router')


instance HasRouter Router where
  router = id

