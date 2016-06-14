module Gonimo.TypeBridges where

import           Control.Applicative
import           Gonimo.PSTypes
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.Builder

gonimoBridge :: BridgePart
gonimoBridge = defaultBridge
  <|> (typeName ^== "Family" >> psServerType)
  <|> (typeName ^== "Key" >> psServerType)
  <|> (typeName ^== "Secret" >> psServerType)
  <|> utcTimeBridge

-- There is currently no Generic instance for PureScript's Data.Date,
-- so just use the JSON string in the frontend:
utcTimeBridge :: BridgePart
utcTimeBridge = do
   typeName ^== "UTCTime"
   return psGonimoDate
