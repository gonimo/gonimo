{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Gonimo.SocketAPI.APIVersion
Description : API version handling
Copyright   : (c) Robert Klotzner, 2017
Data and functions for handling API versions.
-}

module Gonimo.SocketAPI.APIVersion where

import           Data.Text (Text)

-- | API version numbers:
data APIVersion = V1 -- ^ Currently only one version

-- | Current 'APIVersion'
currentVersion :: APIVersion
currentVersion = V1

toText :: APIVersion -> Text
toText V1 = "V1"

fromText :: Text -> Maybe APIVersion
fromText "V1" = Just V1
fromText _    = Nothing


