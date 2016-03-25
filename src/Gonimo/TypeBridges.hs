module Gonimo.TypeBridges where

import           Control.Applicative
import           Gonimo.PSTypes
import           Language.PureScript.Bridge

gonimoBridge :: TypeBridge
gonimoBridge t = defaultBridge t
  <|> secretBridge t
  <|> utcTimeBridge t
  <|> dbKeyBridge t

secretBridge :: TypeBridge
secretBridge = mkBridgeTo (eqTypeName "Secret") psGonimoSecret

-- There is currently no Generic instance for PureScript's Data.Date,
-- so just use the JSON string in the frontend:
utcTimeBridge :: TypeBridge
utcTimeBridge = mkBridgeTo (eqTypeName "UTCTime") psGonimoDate

dbKeyBridge :: TypeBridge
dbKeyBridge = mkBridgeTo1 (eqTypeName "Key") psGonimoDbKey
