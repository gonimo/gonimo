module Gonimo.CodeGen.PSTypes where

import           Language.PureScript.Bridge.TypeInfo
import           Language.PureScript.Bridge.Builder


import Control.Lens
import Control.Monad.Reader.Class

-- PureScript's Data.Date currently has no Generic instance ...
psGonimoDate :: TypeInfo 'PureScript
psGonimoDate = TypeInfo {
    _typePackage = ""
  , _typeModule = "Gonimo.Types"
  , _typeName = "Date"
  , _typeParameters = []
  }


-- | Use type definition in Gonimo.Types
psClientType :: MonadReader BridgeData m => m PSType
psClientType = do
  inType <- view haskType
  params <- psTypeParameters
  return TypeInfo {
    _typePackage = ""
  , _typeModule  = "Gonimo.Types"
  , _typeName = inType ^. typeName
  , _typeParameters = params
  }
