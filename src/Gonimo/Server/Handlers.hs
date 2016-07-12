module Gonimo.Server.Handlers where

import           Control.Monad.Freer           (Eff)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Gonimo.Database.Effects       as Db
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects         hiding (Server)
import           Gonimo.Server.Types
import           Gonimo.Server.Error
import qualified Gonimo.WebAPI.Types as Client
import           Servant                       (ServantErr (..))

-- | Create an anonymous account and a client.
--   Each device is uniquely identified by a ClientId, multiple
--   Client's can share a single account in case a user login was provided,
--   otherwise every client corresponds to a single Account.
createClient :: ServerConstraint r => Maybe Text -> Eff r Client.AuthData
createClient mUserAgent = do
  now <- getCurrentTime
  authToken <- GonimoSecret <$> generateSecret
  runDb $ do
    aid <- Db.insert Account { accountCreated  = now
                             }
    cid <- Db.insert Client  { clientName = fromMaybe "Client" mUserAgent
                             , clientAuthToken = authToken
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
