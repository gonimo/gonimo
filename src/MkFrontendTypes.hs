

import Language.PureScript.Bridge

import Gonimo.Types
import Gonimo.Server.DbTypes
import qualified Data.Map as M
import Data.Proxy

myTypes :: [SumType]
myTypes = [
        toSumType (Proxy :: Proxy UserName)
      , toSumType (Proxy :: Proxy Credentials)
      , toSumType (Proxy :: Proxy AccountData)
      , toSumType (Proxy :: Proxy AuthToken)
      ]

main :: IO ()
main = writePSTypes defaultBridge "../gonimo-front/src" myTypes
