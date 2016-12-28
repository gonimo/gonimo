module Utils.Control.Monad.Trans.Maybe where

import Control.Monad.Trans.Maybe

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
