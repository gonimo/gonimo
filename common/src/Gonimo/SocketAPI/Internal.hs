{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Gonimo.SocketAPI.Internal
Description : Types and internal functions for "Gonimo.SocketAPI"
Copyright   : (c) Robert Klotzner, 2018
-}

module Gonimo.SocketAPI.Internal where

import           Data.Int         (Int64)


-- | Datatype to be used for Ids which are translateable to database ids.
type DbKey = Int64
