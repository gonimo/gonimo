{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | Reflex helper functions
module Gonimo.Client.Reflex where

import Reflex.Dom
import Data.Maybe (isJust, isNothing)


waitForReady :: forall t a. Reflex t => Dynamic t (Maybe a) -> Event t (Dynamic t a)
waitForReady sig = push (\mVal -> do
                            prev <- sample $ current sig
                            if isNothing prev && isJust mVal
                              then sequence (holdDyn <$> mVal <*> Just (push (pure . id ) (updated sig)))
                              else pure Nothing
                        ) (updated sig)
