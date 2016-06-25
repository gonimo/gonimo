module Gonimo.CodeGen.TypeBridges where

import           Control.Applicative
import           Language.PureScript.Bridge

import           Gonimo.CodeGen.PSTypes

gonimoBridge :: BridgePart
gonimoBridge = defaultBridge
  <|> (typeName ^== "Family" >> psClientType)
  <|> (typeName ^== "Key" >> psClientType)
  <|> (typeName ^== "Secret" >> psClientType)
  <|> utcTimeBridge

-- There is currently no Generic instance for PureScript's Data.Date,
-- so just use the JSON string in the frontend:
utcTimeBridge :: BridgePart
utcTimeBridge = do
   typeName ^== "UTCTime"
   return psGonimoDate
