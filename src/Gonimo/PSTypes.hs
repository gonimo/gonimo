module Gonimo.PSTypes where

import           Language.PureScript.Bridge.TypeInfo

-- PureScript's Data.Date currently has no Generic instance ...
psGonimoDate :: TypeInfo
psGonimoDate = TypeInfo {
    typePackage = ""
  , typeModule = "Gonimo.Server.Types"
  , typeName = "Date"
  , typeParameters = []
  }

-- Don't bother converting to a binary format at the client side:
psGonimoSecret :: TypeInfo
psGonimoSecret = TypeInfo {
    typePackage = ""
  , typeModule = "Gonimo.Server.Types"
  , typeName = "Secret"
  , typeParameters = []
  }

psGonimoDbKey :: TypeInfo -> TypeInfo
psGonimoDbKey t = t {
    typePackage = ""
  , typeModule = "Gonimo.Server.Types"
  , typeName = "Key"
  }
