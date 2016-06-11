

import           Language.PureScript.Bridge

import           Data.Proxy
import           GHC.Generics
import           Gonimo.Server.DbEntities
import           Gonimo.Server.DbTypes
import           Gonimo.TypeBridges
import           Gonimo.Types


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

main :: IO ()
main = writePSTypes "../gonimo-front/src" (buildBridge gonimoBridge) myTypes
