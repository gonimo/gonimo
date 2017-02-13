-- Common imports and definitions for gonimo ...
module Gonimo.Prelude ( MaybeT(..)
                      , MonadIO
                      , liftIO
                      , lift
                      , traverse_
                      ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
