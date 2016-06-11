module Gonimo.TypeBridges where

import           Control.Applicative
import           Gonimo.PSTypes
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.Builder

gonimoBridge :: BridgePart
gonimoBridge = defaultBridge
  <|> secretBridge
  <|> utcTimeBridge
  <|> dbKeyBridge

secretBridge :: BridgePart
secretBridge = do
   typeName ^== "Secret"
   return psGonimoSecret

-- There is currently no Generic instance for PureScript's Data.Date,
-- so just use the JSON string in the frontend:
utcTimeBridge :: BridgePart
utcTimeBridge = do
   typeName ^== "UTCTime"
   return psGonimoDate

dbKeyBridge :: BridgePart
dbKeyBridge = do
  typeName ^== "Key"
  psGonimoDbKey
