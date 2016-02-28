module Gonimo.Server.Handlers where

import Control.Monad.Freer (Eff)
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Types
import Servant (ServantErr(..))
import qualified Gonimo.Database.Effects as Db
import Gonimo.Util

createAccount :: ServerConstraint r => Maybe Credentials -> Eff r (AccountId, AuthToken)
createAccount mcred = do
  now <- getCurrentTime
  let email = mcred >>= getUserEmail . userName
  let phone = mcred >>= getUserPhone . userName
  let password = userPassword <$> mcred
  asecret <- generateSecret
  aid <- runDb $ Db.insert Account {
    accountSecret = asecret
    , accountCreated = now
    , accountLastAccessed = now
    , accountEmail = email
    , accountPhone = phone
    , accountPassword = password
    }
  return (aid, GonimoSecret asecret)



getCoffee :: ServerConstraint r => Eff r Coffee
getCoffee = throwServant ServantErr { errReasonPhrase = "I am a tea pot!"
                                                        , errHTTPCode = 418
                                                        , errBody = ""
                                                        , errHeaders = []
                                                        }
