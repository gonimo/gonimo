module Gonimo.Server.Handlers where

import Control.Monad.Freer (Eff)
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Types
import Servant (ServantErr(..))
import qualified Gonimo.Database.Effects as Db
import Gonimo.Util

createAccount :: ServerConstraint r => Maybe Credentials -> Eff r (AccountId, AuthToken)
-- | creating a new "Account" implicitly also generates a "Client" - that is
-- associated to this "Account".
createAccount mcred = do
  now <- getCurrentTime
  let email = mcred >>= getUserEmail . userName
  let phone = mcred >>= getUserPhone . userName
  let password = userPassword <$> mcred
  csecret <- generateSecret
  aid <- runDb $ Db.insert Account { accountCreated  = now
                                   , accountEmail    = email
                                   , accountPhone    = phone
                                   , accountPassword = password
                                   }

  _ <- runDb $ Db.insert Client { clientSecret       = csecret
                                , clientAccountId    = aid
                                , clientLastAccessed = now}
  return (aid, GonimoSecret csecret)



getCoffee :: ServerConstraint r => Eff r Coffee
getCoffee = throwServant ServantErr { errReasonPhrase = "I am a tea pot!"
                                                        , errHTTPCode = 418
                                                        , errBody = ""
                                                        , errHeaders = []
                                                        }
