module Gonimo.WebAPI where

import Servant.API
import Data.Proxy
import Gonimo.Types


type GonimoAPI =
  -- Create an account pass Nothing if you want an anonymous account:
  "accounts" :> ReqBody '[JSON] (Maybe Credentials) :> Post '[JSON] AuthToken
  :<|> "coffee" :> Get '[JSON] Coffee

gonimoAPI :: Proxy GonimoAPI
gonimoAPI = Proxy
       
gonimoLink :: (IsElem endpoint GonimoAPI, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
gonimoLink = safeLink gonimoAPI
