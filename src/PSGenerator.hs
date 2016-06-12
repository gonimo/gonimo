{-# LANGUAGE OverloadedStrings #-}

import           Language.PureScript.Bridge

import Control.Lens
import           Data.Proxy
import           GHC.Generics
import           Gonimo.Server.DbEntities
import           Gonimo.Server.DbTypes
import           Gonimo.WebAPI
import           Gonimo.TypeBridges
import           Gonimo.Types
import           Servant.PureScript

data GonimoBridge

instance HasBridge GonimoBridge where
  languageBridge _ = buildBridge gonimoBridge


gonimoProxy :: Proxy GonimoBridge
gonimoProxy = Proxy

data TestTypeConstructor m a = TestTypeConstructor (m a) deriving Generic

myTypes :: [SumType 'Haskell]
myTypes = [
        mkSumType (Proxy :: Proxy UserName)
      , mkSumType (Proxy :: Proxy Credentials)
      , mkSumType (Proxy :: Proxy AccountData)
      , mkSumType (Proxy :: Proxy AuthToken)
      , mkSumType (Proxy :: Proxy Invitation)
      , mkSumType (Proxy :: Proxy SendInvitation)
      , mkSumType (Proxy :: Proxy InvitationDelivery)
      ]

mySettings :: Settings
mySettings = addReaderParam "AuthToken" defaultSettings & apiModuleName .~ "Gonimo.WebAPI"


main :: IO ()
main = do
  let gonimoFrontPath = "../gonimo-front/src"
  writePSTypes gonimoFrontPath (buildBridge gonimoBridge) myTypes
  writeAPIModuleWithSettings mySettings gonimoFrontPath gonimoProxy gonimoAPI
