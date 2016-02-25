module Gonimo.Server.Auth where

import Control.Monad.Freer.Reader
import Control.Monad.Freer (send, Member, Eff)
import Gonimo.Server.DbEntities (Account)
import Control.Monad.Freer.Exception (Exc(..))
import Gonimo.Server.Effects (ServerConstraint)


data AuthData = AuthData {
  authDataAccountEntity :: Entity Account
}

type AuthServerConstraint r = (Member (Reader AuthData) r, ServerConstraint r)
type AuthServerEffects = Eff '[Reader (Entity Account), Exc ServerException, Server]
