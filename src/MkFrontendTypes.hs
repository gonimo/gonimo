

import           Language.PureScript.Bridge

import           Data.Proxy
import           GHC.Generics
import           Gonimo.Server.DbEntities
import           Gonimo.Server.DbTypes
import           Gonimo.TypeBridges
import           Gonimo.Types


data TestTypeConstructor m a = TestTypeConstructor (m a) deriving Generic

myTypes :: [SumType]
myTypes = [
        toSumType (Proxy :: Proxy UserName)
      , toSumType (Proxy :: Proxy Credentials)
      , toSumType (Proxy :: Proxy AccountData)
      , toSumType (Proxy :: Proxy AuthToken)
      , toSumType (Proxy :: Proxy Invitation)
      , toSumType (Proxy :: Proxy SendInvitation)
      , toSumType (Proxy :: Proxy InvitationDelivery)
      ]

main :: IO ()
main = writePSTypes gonimoBridge "../gonimo-front/src" myTypes
