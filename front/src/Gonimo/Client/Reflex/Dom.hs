{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex.Dom where

import Reflex.Dom
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Debug.Trace (trace)

enterPressed :: Reflex t => Event t Int -> Event t ()
enterPressed = push (\key -> pure $ if key == 13
                                    then Just ()
                                    else Nothing
                    )
