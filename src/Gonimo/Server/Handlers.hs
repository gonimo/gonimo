module Gonimo.Server.Handlers where

import Control.Monad.Freer (Eff)
import Control.Monad.Trans.Either (EitherT(..), left)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Gonimo.Server.DbEntities
import Gonimo.Server.DbTypes
import Gonimo.Server.Effects hiding (Server)
import Gonimo.Server.Effects.TestServer
import Gonimo.Types
import Gonimo.WebAPI
import Servant (ServantErr(..), err500, Server, (:<|>)(..), ServerT, enter, (:~>)(..), utf8Encode)
import qualified Gonimo.Database.Effects as Db
import qualified Data.Text as T
import Servant.Server (err404, err400)
import Database.Persist (Entity(..))
import Gonimo.Server.EmailInvitation
import Control.Monad.Freer.Exception (throwError)



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
