{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Account.Impl
Description : Types, classes and utilities for dealing with models and model configs.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Model ( -- * Types & Constraint types
                             IsConfig
                           ) where

import Data.Default
import Data.Semigroup
import Reflex.Class.Extended

-- | Types that have the properties that we demand from configurations.
--
--   Types must be a Monoid, so we can combine configs and have a default
--   config. They should also have a Default instance, with def being mempty.
--   Also they need to be Flattenable so we can return them from dynamic views.
type IsConfig c t = (Default (c t), Monoid (c t), Flattenable c, Semigroup (c t))
