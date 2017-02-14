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
                      , Text
                      ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe as Maybe (fromMaybe, maybe, isJust, isNothing)
import Safe as Safe (headMay)
import Data.Text (Text)
import Data.Monoid as Monoid
import Control.Applicative as Applicative
