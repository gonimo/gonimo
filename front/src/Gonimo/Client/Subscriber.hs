{-|
Module      : Gonimo.Client.Subscriber
Description : User facing Subscriber API
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Subscriber where

import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Gonimo.Client.Model   (IsConfig)
import           Gonimo.Client.Prelude
import qualified Gonimo.SocketAPI      as API


{-# Deprecated SubscriptionsDyn " Use plain 'Subscriber' instead."#-}
type SubscriptionsDyn t = Dynamic t (Set API.ServerRequest)

-- | Configuration for creating an subscriber.
--
--   Currently this just handles accepting invitations.
data Config t
  = Config { -- | The 'ServerRequest's you want to have subscribed
             _subscriptions :: Dynamic t (Set API.ServerRequest)
           } deriving (Generic)

-- | Create subscriptions from server request events.
fromServerRequests :: (Reflex t, MonadHold t m, HasConfig c, IsConfig c t)
                   => Event t [API.ServerRequest] -> m (c t)
fromServerRequests reqs = do
  subs <- holdDyn Set.empty $ Set.fromList <$> reqs
  pure $ mempty & subscriptions .~ subs

instance Reflex t => Default (Config t) where
  def = mempty

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = memptydefault
  mappend = (<>)

-- Auto generated lenses:

class HasConfig a where
  config :: Lens' (a t) (Config t)

  subscriptions :: Lens' (a t) (Dynamic t (Set API.ServerRequest))
  subscriptions = config . go
    where
      go :: Lens' (Config t) (Dynamic t (Set API.ServerRequest))
      go f config' = (\subscriptions' -> config' { _subscriptions = subscriptions' }) <$> f (_subscriptions config')


instance HasConfig Config where
  config = id


instance Flattenable Config where
  flattenWith doSwitch ev = Config <$> flattenDynamic doSwitch (_subscriptions <$> ev)
