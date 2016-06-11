module Gonimo.PSTypes where

import           Language.PureScript.Bridge.TypeInfo
import Control.Monad.Reader.Class

-- PureScript's Data.Date currently has no Generic instance ...
psGonimoDate :: TypeInfo 'PureScript
psGonimoDate = TypeInfo {
    _typePackage = ""
  , _typeModule = "Gonimo.Server.Types"
  , _typeName = "Date"
  , _typeParameters = []
  }

-- Don't bother converting to a binary format at the client side:
psGonimoSecret :: TypeInfo 'PureScript
psGonimoSecret = TypeInfo {
    _typePackage = ""
  , _typeModule = "Gonimo.Server.Types"
  , _typeName = "Secret"
  , _typeParameters = []
  }

psGonimoDbKey :: MonadReader BridgeData m => m PSType
psGonimoDbKey = TypeInfo "" "Gonimo.Server.Types" "Key" <$> psTypeParameters
