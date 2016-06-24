module Gonimo.Server.Handlers where

import Control.Monad.Freer (Eff)
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Types
import Servant (ServantErr(..))
import qualified Gonimo.Database.Effects as Db
import Gonimo.Util
import qualified Gonimo.Client.Types as Client

-- | Create an anonymous account and a client.
--   Each device is uniquely identified by a ClientId, multiple
--   Client's can share a single account in case a user login was provided,
--   otherwise every client corresponds to a single Account.
createClient :: ServerConstraint r => Eff r Client.AuthData
createClient = do
  now <- getCurrentTime
  authToken <- GonimoSecret <$> generateSecret
  runDb $ do
    aid <- Db.insert Account { accountCreated  = now
                             }
    cid <- Db.insert Client  { clientAuthToken = authToken
                             , clientAccountId    = aid
                             , clientLastAccessed = now
                             }
    return Client.AuthData {
        Client.accountId = aid
      , Client.clientId = cid
      , Client.authToken = authToken
      }



getCoffee :: ServerConstraint r => Eff r Coffee
getCoffee = throwServant ServantErr { errReasonPhrase = "I am a tea pot!"
                                                        , errHTTPCode = 418
                                                        , errBody = ""
                                                        , errHeaders = []
                                                        }
