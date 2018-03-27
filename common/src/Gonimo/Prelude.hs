-- Common imports and definitions for gonimo ...
module Gonimo.Prelude ( MaybeT(..)
                      , MonadIO
                      , liftIO
                      , lift
                      , traverse_
                      , module Maybe
                      , module Safe
                      , module Semigroup
                      , module Applicative
                      , module Monad
                      , Text
                      , module Constants
                      , module Control.Exception
                      , module Control.Lens
                      , module Data.Default
                      , module Arrow
                      , module Generics.Deriving.Base
                      , module Generics.Deriving.Semigroup
                      , module Generics.Deriving.Monoid
                      , module Control.Monad.Reader.Class
                      ) where

import           Control.Applicative       as Applicative
import           Control.Arrow             as Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad             as Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Default
import           Data.Foldable             (traverse_)
import           Data.Maybe                as Maybe (catMaybes, fromMaybe,
                                                     isJust, isNothing,
                                                     mapMaybe, maybe, listToMaybe)
import           Data.Semigroup            as Semigroup
import           Data.Text                 (Text)
import           Generics.Deriving.Base    (Generic)
import           Generics.Deriving.Semigroup (gsappenddefault)
import           Generics.Deriving.Monoid  (memptydefault, mappenddefault)
import           Gonimo.Constants          as Constants
import           Safe                      as Safe (headMay)
import           Control.Monad.Reader.Class
