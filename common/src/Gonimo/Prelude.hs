-- Common imports and definitions for gonimo ...
module Gonimo.Prelude ( MaybeT(..)
                      , MonadIO
                      , liftIO
                      , lift
                      , traverse_
                      , module Maybe
                      , module Safe
                      , module Monoid
                      , module Applicative
                      , module Monad
                      , module Arrow
                      , Text
                      , module Constants
                      , module Control.Exception
                      , module Control.Lens
                      , module Data.Default
                      ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe as Maybe (fromMaybe, maybe, isJust, isNothing, mapMaybe, catMaybes)
import Safe as Safe (headMay)
import Data.Text (Text)
import Data.Monoid as Monoid
import Control.Applicative as Applicative
import Control.Monad as Monad
import Gonimo.Constants as Constants
import Control.Arrow as Arrow
import Control.Exception
import Control.Lens
import Data.Default
