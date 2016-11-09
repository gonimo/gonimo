module Gonimo.Server.Handlers where

import           Control.Monad.Freer           (Eff)
import           Data.Text                     (Text, take, unwords)
import qualified Gonimo.Database.Effects       as Db
import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Effects         hiding (Server)
import           Gonimo.Server.Types
import           Gonimo.Server.Error
import qualified Gonimo.WebAPI.Types as Client
import           Prelude                       hiding (take, unwords)
import           Servant                       (ServantErr (..))
import           Database.Persist              (entityVal, (==.), Entity)
import           Utils.System.Random



noUserAgentDefault :: Text
noUserAgentDefault = "None"

maxUserAgentLength :: Int
maxUserAgentLength = 300


-- | Create an anonymous account and a device.
--   Each device is uniquely identified by a DeviceId, multiple
--   Device's can share a single account in case a user login was provided,
--   otherwise every device corresponds to a single Account.
createDevice :: ServerConstraint r => Maybe Text -> Eff r Client.AuthData
createDevice mUserAgent = do
  now <- getCurrentTime
  authToken <- GonimoSecret <$> generateSecret
  let userAgent = maybe noUserAgentDefault (take maxUserAgentLength) mUserAgent
  funnyName <- createFunnyName
  runDb $ do
    aid <- Db.insert Account { accountCreated     = now
                             }
    cid <- Db.insert Device  { deviceName         = funnyName
                             , deviceAuthToken    = authToken
                             , deviceAccountId    = aid
                             , deviceLastAccessed = now
                             , deviceUserAgent    = userAgent
                             }
    return Client.AuthData {
        Client.accountId = aid
      , Client.deviceId  = cid
      , Client.authToken = authToken
      }


-- | Generate a funny user name.
--   You may this part of the API by running the shell command:
--   curl --request POST http://localhost:8081/funnyName
createFunnyName :: ServerConstraint r => Eff r Text
createFunnyName = do
  let scaffold    = [FunnyPrefix, FunnyPrefix, FunnyCharacter, FunnySuffix]
      fetch t     = Db.selectList [FunnyWordWordType ==. t] []
      word       :: Entity FunnyWord -> Text
      word        = funnyWordWord . entityVal
  funnyWordPools <- map (map word) <$>
                      (runDb $ mapM fetch scaffold)
  funnyWords     <- runRandom $ randomLs funnyWordPools
  return $ unwords funnyWords


getCoffee :: ServerConstraint r => Eff r Coffee
getCoffee = throwServant ServantErr { errReasonPhrase = "I am a tea pot!"
                                                        , errHTTPCode = 418
                                                        , errBody = ""
                                                        , errHeaders = []
                                                        }
